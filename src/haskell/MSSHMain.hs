{-# LANGUAGE DeriveDataTypeable #-}

module Main (
  main
) where

import Control.Concurrent
import System.Console.CmdArgs
import Control.Applicative
import System.Process
import System.IO
import System.Console.Haskeline
import System.Environment
import System.Directory
import Control.Exception (AsyncException(..))
import Control.Monad.IO.Class

--import qualified Data.Text as T
--import qualified Data.Text.IO as TIO



data MSSHOpts =
  MSSHOpts { foo:: Bool
           , servers:: [String]
           }
           deriving(Show, Data, Typeable)


mSSHOpts :: MSSHOpts
mSSHOpts =
  MSSHOpts { foo = False &= help "foo"
           , servers = [] &= args
           } &=
              program "mssh" &=
              help ("Drive multiple SSH sessions with the same keyboard input\n"++
                    "server1 server[2,3] server[4,5..9]")


consumeOutput:: String -> Handle -> IO ()
consumeOutput prefix h = do
  theEnd <- hIsEOF h
  if (not theEnd)
    then do
      l <- hGetLine h
      putStr $ "\r" ++ prefix ++ l ++ "\n\r" ++ msshPrompt
      hFlush stdout
      consumeOutput prefix h
    else
      return ()

startSSH:: MSSHOpts -> String -> IO (Handle, ProcessHandle)
startSSH opts host = do
  (hIn,hOut,hErr,hProc) <- runInteractiveCommand $ "ssh -t " ++ host
  hSetBuffering hIn LineBuffering
  hSetBuffering hOut LineBuffering
  hSetBuffering hErr LineBuffering
  forkIO $ consumeOutput (host ++ "> ") hOut
  forkIO $ consumeOutput (host ++ "! ") hErr
  return (hIn, hProc)


msshHistoryFile:: IO FilePath
msshHistoryFile = do
  hDir <- getHomeDirectory
  return $ hDir ++ "/.mssh"


msshPrompt:: String
msshPrompt = "?> "


main :: IO ()
main = do
    opts <- cmdArgs mSSHOpts
    putStrLn $ show opts

    inputAndProcHandles <- mapM (startSSH opts) (servers opts)
    let hsIn = map (fst) inputAndProcHandles
    let hsProc = map (snd) inputAndProcHandles

    haskelineSettings <- fmap (\hf -> defaultSettings { historyFile = Just hf }) msshHistoryFile

    runInputT haskelineSettings$ withInterrupt $ loop hsIn

    mapM_ waitForProcess hsProc

  where
    sendInputLine:: [Handle] -> String -> IO ()
    sendInputLine hsIn line = do
      _ <- mapM_ ((flip hPutStrLn) line) hsIn
      mapM_ (hFlush) hsIn

    loop:: [Handle] -> InputT IO ()
    loop hsIn = do
      minput <- handle (\Interrupt -> return Nothing)
                       $ getInputLine "?> "
      case minput of
        Nothing -> do
          liftIO $ mapM_ hClose hsIn
        Just l -> do
          _ <- liftIO $ sendInputLine hsIn l
          loop hsIn
