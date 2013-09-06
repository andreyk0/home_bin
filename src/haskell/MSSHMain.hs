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
import Text.Regex.TDFA

import Data.Maybe

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
  (hIn,hOut,hErr,hProc) <- runInteractiveCommand $ "ssh -T " ++ host
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

-- foo1,2,3:5,11 -->
--  foo1 foo2 foo3 foo4 foo5 foo11
expandServerNames:: String -> [String]
expandServerNames sn =
  case listToMaybe $ splitBaseNameAndNumRanges sn of
    Just (_:baseName:numRanges:suffix:[]) ->
      map (flip (++) suffix) $ map ((++) baseName) $ concat $ map (expandNumRange) $ splitNumRanges numRanges
    Just _ -> [sn]
    Nothing -> [sn]

  where
    -- ghci> "foo1,2,3:5,11.bar.baz" =~ "([^0-9,:]+)([0-9,:]+)(\\..+)?" :: [[String]]
    -- [["foo1,2,3:5,11.bar.baz","foo","1,2,3:5,11",".bar.baz"]]
    splitBaseNameAndNumRanges:: String -> [[String]]
    splitBaseNameAndNumRanges s = s =~ "([^0-9,:]+)([0-9,:]+)(\\..+)?"

    -- ghci> "1,2:3,4,5" =~ "[^,]+" :: [[String]]
    -- [["1"],["2:3"],["4"],["5"]]
    splitNumRanges:: String -> [String]
    splitNumRanges nr = map (head) (nr =~ "[^,]+" :: [[String]])

    -- 1:3 -> [1,2,3]
    -- 13 -> [13]
    expandNumRange:: String -> [String]
    expandNumRange nr = case nums of
        from:to:[]      -> map (show) [ from .. to ]
        from:to:step:[] -> map (show) [ from, from+step .. to ]
        _ -> [nr]
      where nums = map (read . head) (nr =~ "[^:]+" :: [[String]]) :: [Int]


main :: IO ()
main = do
    opts <- cmdArgs mSSHOpts
    putStrLn $ show opts

    let allServerNames = concat $ map (expandServerNames) $ servers opts

    inputAndProcHandles <- mapM (startSSH opts) allServerNames
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
