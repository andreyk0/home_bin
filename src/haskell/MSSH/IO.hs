module MSSH.IO (
  OutEvt,
  Tee,
  awaitTermination,
  consumeStdErr,
  consumeStdOut,
  newTee,
  runTee
) where

import Control.Concurrent
import System.IO

import qualified Control.Concurrent.MSemN as Sem
import qualified Data.Traversable as T

import Util.AnsiColor

-- | Represents a line of output or an end of stream condition
data OutEvt = StdOutLine { hostName:: String
                         , outLine:: String
                         }
             | StdErrLine { hostName:: String
                          , outLine:: String
                          }
             | EndOfStream
             | Exit
             deriving(Show)

data Tee = Tee { teePrompt:: String -- ^ prompt (need to restore after printing output lines)
               , teeColors:: (AnsiColor, AnsiColor) -- ^ stdout/err colors
               , teePadHostNames:: (String -> String) -- ^ pad hostnames to the same width
               , teeIn:: MVar OutEvt -- ^ take lines of text from here
               , teeOutSem:: Sem.MSemN Integer -- sync up with the main thread, increment end of stream counters
               , teeSaveFH:: Maybe Handle -- ^ 'save' file handle to append output to
               }

newTee:: String -- ^ prompt (need to restore after printing output lines)
      -> (AnsiColor, AnsiColor) -- ^ stdout/err colors
      -> (String -> String) -- ^ pad hostnames to the same width
      -> Maybe FilePath -- ^ 'save' file to append output to
      -> IO Tee
newTee msshPrompt colors padHostnames maybeSaveFilePath = do
    saveFileFH <- T.forM maybeSaveFilePath $ \fp -> do
      fh <- openFile fp WriteMode
      return fh
    teeInMVar <- newEmptyMVar
    endOfOutputSem <- Sem.new 0 -- in the end this should go up to num hosts
    return $ Tee { teePrompt = msshPrompt
                 , teeColors = colors
                 , teePadHostNames  = padHostnames
                 , teeIn = teeInMVar
                 , teeOutSem = endOfOutputSem
                 , teeSaveFH = saveFileFH
                 }

awaitTermination:: Tee -> Integer -> IO ()
awaitTermination t numSSHStreams = do
    Sem.wait (teeOutSem t) numSSHStreams -- wait for all SSH output to stop
    signalExit (teeIn t) -- stop the console printing loop
    Sem.wait (teeOutSem t) 1 -- wait for console printing loop to exit
    _ <- T.forM (teeSaveFH t) hClose -- close 'save' file
    return()

-- | Prints lines of text to screen
--   and optionally saves to output file
runTee:: Tee -> IO ()
runTee t = do
    ol <- takeMVar (teeIn t)
    let (stdoutColor, stderrColor) = teeColors t
    let handleLine = \color outPrompt -> do
        let prefix = ansiColor color $ ((teePadHostNames t) . hostName) ol ++ outPrompt
        putStr $ "\r" ++ prefix ++ (outLine ol) ++ "\n\r" ++ (teePrompt t)
        hFlush stdout
        _ <- T.forM (teeSaveFH t) $ \fh -> do
          hPutStrLn fh $ (hostName ol) ++ ":"++ (outLine ol)
        return ()

    let loop = runTee t
    let incExitCount = Sem.signalF (teeOutSem t) (\i -> (1, i)) -- increment by 1

    case ol of
      StdOutLine _ _ -> do handleLine stdoutColor "> "
                           loop
      StdErrLine _ _ -> do handleLine stderrColor "! "
                           loop
      EndOfStream  -> do _ <- incExitCount
                         loop
      Exit -> do _ <- incExitCount
                 return ()

-- | sends a special Exit OutEvt to break the loop
signalExit:: MVar OutEvt -> IO ()
signalExit mv = do putMVar mv Exit

-- | Consumes stdout lines
consumeStdOut:: String -- ^ hostName
             -> Handle -- ^ file handle to read data from
             -> Tee -- ^ send consumed lines here
             -> IO ()
consumeStdOut hn = consumeOutputLine $ (StdOutLine) hn

-- | Consumes stderr lines
consumeStdErr:: String -- ^ hostName
             -> Handle -- ^ file handle to read data from
             -> Tee -- ^ send consumed lines here
             -> IO ()
consumeStdErr hn = consumeOutputLine $ (StdErrLine) hn

-- | Consumes output from a given file handle,
--   sends to an MVar, another IO thread
--   will read from it and print results, this
--   way we line-buffer and avoid interleaving lines
consumeOutputLine:: (String -> OutEvt) -- ^ constructor for the kind of stream we consume
                 -> Handle -- ^ file handle to read data from
                 -> Tee -- ^ send consumed lines here
                 -> IO ()
consumeOutputLine mkOutEvt h t = do
  let outMVar = teeIn t
  theEnd <- hIsEOF h
  if (not theEnd)
    then do
      l <- hGetLine h
      putMVar outMVar $ mkOutEvt l
      consumeOutputLine (mkOutEvt) h t
    else
      putMVar outMVar EndOfStream
