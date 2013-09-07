{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Text.Printf
import Text.Regex.TDFA
import TupleTH

data MSSHOpts =
  MSSHOpts { sshCommand:: String
           , color:: Bool
           , outFile:: Maybe FilePath
           , hosts:: [String]
           }
           deriving(Show, Data, Typeable)

mSSHOpts :: MSSHOpts
mSSHOpts =
  MSSHOpts { sshCommand = "ssh -T" &= help "ssh command to run for each hostname, defaults to 'ssh -T'"
           , color = False &= help "turn on color output"
           , outFile = Nothing &= help "save output to file"
           , hosts = [] &= args
           } &=
              program "mssh" &=
                summary "Drive multiple SSH sessions with the same keyboard input" &=
                help "Usage: mssh host1 host2.foo.bar host4,5 host6,7:9 host10:20:2"

data AnsiColor = Black | Red | Green | Yellow  | Blue  | Magenta | Cyan  | White | Disabled
  deriving (Show, Enum)

-- | Quick color-printing hack
ansiColor:: AnsiColor -> String -> String
ansiColor c s = case c of
  Disabled -> s
  _ -> printf "\x1b[%d;2m%s\x1b[0m" (30+ (fromEnum c)) s

-- | Consumes output from a given file handle,
--   prints to screen and (optionally) saves to output file
consumeOutput:: String -- ^ output prefix
             -> Handle -- ^ file handle to read data from
             -> Maybe (String, Handle) -- ^ line prefix / file handle to optionally save output to
             -> MVar () -- ^ empty MVar to sync up with the main thread when all output is written
             -> IO ()
consumeOutput prefix h saveFileH finishedOutputMV = do
  theEnd <- hIsEOF h
  if (not theEnd)
    then do
      l <- hGetLine h
      case saveFileH of
        Just (pfx, fh) -> hPutStrLn fh $ pfx ++ l
        Nothing -> return ()
      putStr $ "\r" ++ prefix ++ l ++ "\n\r" ++ msshPrompt
      hFlush stdout
      consumeOutput prefix h saveFileH finishedOutputMV
    else
      putMVar finishedOutputMV ()

-- | Starts SSH and output printing IO threads
startSSH:: MSSHOpts           -- ^ command line opts
        -> (String -> String) -- ^ host name to prefix conversion func, for easy currying
        -> Maybe Handle       -- ^ optional 'save file' output handle
        -> String             -- ^ host name to connect to
        -> IO (Handle, ProcessHandle, [MVar ()]) -- ^ file handle to write commands to (input),
                                                 --  process handle (to wait for process to stop)
                                                 --  MVars to sync up with output consumer threads (wait for them all to finish)
startSSH opts hostNameToPrefix maybeSaveFileH host = do
  (hIn,hOut,hErr,hProc) <- runInteractiveCommand $ (sshCommand opts) ++ " " ++ host
  hSetBuffering hIn LineBuffering
  hSetBuffering hOut LineBuffering
  hSetBuffering hErr LineBuffering
  let outColor = ansiColor $ if (color opts) then Cyan else Disabled
  let errColor = ansiColor $ if (color opts) then Red else Disabled
  let prefix = hostNameToPrefix host
  finishedOutMV <- newEmptyMVar
  finishedErrMV <- newEmptyMVar
  let saveFileLinePrefix = host ++ ":" -- for easy sed, etc parsing
  let saveFilePrefixAndFh = fmap (\fh -> (saveFileLinePrefix,fh)) maybeSaveFileH
  _ <- forkIO $ consumeOutput (outColor $ prefix ++ "> ") hOut saveFilePrefixAndFh finishedOutMV
  _ <- forkIO $ consumeOutput (errColor $ prefix ++ "! ") hErr saveFilePrefixAndFh finishedErrMV
  return (hIn, hProc, [finishedOutMV, finishedErrMV])

-- | Path to command history file
msshHistoryFile:: IO FilePath
msshHistoryFile = do
  hDir <- getHomeDirectory
  return $ hDir ++ "/.mssh"

msshPrompt:: String
msshPrompt = "mssh> "

-- | Parse host name shortcuts and expand them to full host names.
--   E.g. foo1,2,3:5,11 --> foo1 foo2 foo3 foo4 foo5 foo11
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

-- | TODO: Hmm, there's got to be a cleaner way
maybeIO:: Maybe a -> (a -> IO b) -> IO (Maybe b)
maybeIO x fx = do
  case x of
    Nothing -> return Nothing
    Just v -> do
      res <- fx v
      return $ Just res

main :: IO ()
main = do
    opts <- cmdArgs mSSHOpts

    saveFileFh <- maybeIO (outFile opts) $ \fp -> do
      fh <- openFile fp WriteMode
      hSetBuffering fh LineBuffering
      return fh

    let allHostNames = concat $ map (expandHostNames) $ hosts opts
    let maxHostNameLen = maximum $ map (length) allHostNames
    let padToMaxHostNameLen = (\h -> h ++ (take (maxHostNameLen - length h) $ repeat ' '))
    let startSSHForHost = startSSH opts padToMaxHostNameLen saveFileFh

    inputAndHandles <- mapM (startSSHForHost) allHostNames
    let hsIn = map $(proj 3 0) inputAndHandles
    let hsProc = map $(proj 3 1) inputAndHandles
    let finishedOutMVs = concat $ map $(proj 3 2) inputAndHandles

    haskelineSettings <- fmap (\hf -> defaultSettings { historyFile = Just hf }) msshHistoryFile
    runInputT haskelineSettings$ withInterrupt $ loop hsIn

    mapM_ waitForProcess hsProc -- wait for all procs to stop
    mapM_ (takeMVar) finishedOutMVs -- wait for all output printing threads to stop
    _ <- maybeIO saveFileFh hClose -- finish output to save file
    return ()

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
