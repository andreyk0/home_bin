{-# LANGUAGE DeriveDataTypeable #-}

module Main (
  main
) where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.Maybe
import System.Console.CmdArgs
import System.Console.Haskeline
import System.Directory
import System.IO
import System.Process
import Text.Regex.TDFA

data MSSHOpts =
  MSSHOpts { sshCommand:: String
           , hosts:: [String]
           }
           deriving(Show, Data, Typeable)

mSSHOpts :: MSSHOpts
mSSHOpts =
  MSSHOpts { sshCommand = "ssh -T" &= help "ssh command to run for each hostname, defaults to 'ssh -T'"
           , hosts = [] &= args
           } &=
              program "mssh" &=
                summary "Drive multiple SSH sessions with the same keyboard input" &=
                help "Usage: mssh host1 host2.foo.bar host4,5 host6,7:9 host10:20:2"

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
  (hIn,hOut,hErr,hProc) <- runInteractiveCommand $ (sshCommand opts) ++ " " ++ host
  hSetBuffering hIn LineBuffering
  hSetBuffering hOut LineBuffering
  hSetBuffering hErr LineBuffering
  _ <- forkIO $ consumeOutput (host ++ "> ") hOut
  _ <- forkIO $ consumeOutput (host ++ "! ") hErr
  return (hIn, hProc)

msshHistoryFile:: IO FilePath
msshHistoryFile = do
  hDir <- getHomeDirectory
  return $ hDir ++ "/.mssh"

msshPrompt:: String
msshPrompt = "mssh> "

-- foo1,2,3:5,11 -->
--  foo1 foo2 foo3 foo4 foo5 foo11
expandHostNames:: String -> [String]
expandHostNames sn =
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
    let allHostNames = concat $ map (expandHostNames) $ hosts opts

    inputAndProcHandles <- mapM (startSSH opts) allHostNames
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
                       $ getInputLine msshPrompt
      case minput of
        Nothing -> do
          liftIO $ mapM_ hClose hsIn
        Just l -> do
          _ <- liftIO $ sendInputLine hsIn l
          loop hsIn
