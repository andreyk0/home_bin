module MSSH.IO (
  OutLine,
  consumeStdOut,
  consumeStdErr,
  signalExit,
  tee
) where

import Control.Concurrent
import System.IO

import qualified Control.Concurrent.MSemN as Sem
import qualified Data.Traversable as T

import Util.AnsiColor

-- | Represents a line of output or an end of stream condition
data OutLine = StdOutLine { hostName:: String
                          , outLine:: String
                          }
             | StdErrLine { hostName:: String
                          , outLine:: String
                          }
             | EndOfStream
             | Exit
             deriving(Show)

-- | Prints lines of text to screen
--   and optionally saves to output file
tee:: String -- ^ prompt (need to restore after printing output lines)
   -> (AnsiColor, AnsiColor) -- ^ stdout/err colors
   -> (String -> String) -- ^ pad hostnames to the same width
   -> MVar OutLine -- ^ take lines of text from here
   -> Sem.MSemN Integer -- sync up with the main thread, increment end of stream counters
   -> Maybe Handle -- ^ 'save' file handle to append output to
   -> IO ()
tee msshPrompt colors padHostname inMV eosSem maybeSaveFh = do
    ol <- takeMVar inMV
    let (stdoutColor, stderrColor) = colors
    let handleLine = \color outPrompt -> do
        let prefix = ansiColor color $ (padHostname . hostName) ol ++ outPrompt
        putStr $ "\r" ++ prefix ++ (outLine ol) ++ "\n\r" ++ msshPrompt
        hFlush stdout
        _ <- T.forM maybeSaveFh $ \fh -> do
          hPutStrLn fh $ (hostName ol) ++ ":"++ (outLine ol)
        return ()

    let loop = tee msshPrompt colors (padHostname) inMV eosSem maybeSaveFh
    let incExitCount = Sem.signalF eosSem (\i -> (1, i)) -- increment by 1

    case ol of
      StdOutLine _ _ -> do handleLine stdoutColor "> "
                           loop
      StdErrLine _ _ -> do handleLine stderrColor "! "
                           loop
      EndOfStream  -> do _ <- incExitCount
                         loop
      Exit -> do _ <- incExitCount
                 return ()

-- | sends a special Exit OutLine to break the loop
signalExit:: MVar OutLine -> IO ()
signalExit mv = do putMVar mv Exit

-- | Consumes stdout lines
consumeStdOut:: String -- ^ hostName
             -> Handle -- ^ file handle to read data from
             -> MVar OutLine -- ^ we write consumed lines to this MVar
             -> IO ()
consumeStdOut hn = consumeOutputLine $ (StdOutLine) hn

-- | Consumes stderr lines
consumeStdErr:: String -- ^ hostName
             -> Handle -- ^ file handle to read data from
             -> MVar OutLine -- ^ we write consumed lines to this MVar
             -> IO ()
consumeStdErr hn = consumeOutputLine $ (StdErrLine) hn

-- | Consumes output from a given file handle,
--   sends to an MVar, another IO thread
--   will read from it and print results, this
--   way we line-buffer and avoid interleaving lines
consumeOutputLine:: (String -> OutLine) -- ^ constructor for the kind of stream we consume
                 -> Handle -- ^ file handle to read data from
                 -> MVar OutLine -- ^ we write consumed lines to this MVar
                 -> IO ()
consumeOutputLine mkOutLine h outMVar = do
  theEnd <- hIsEOF h
  if (not theEnd)
    then do
      l <- hGetLine h
      putMVar outMVar $ mkOutLine l
      consumeOutputLine (mkOutLine) h outMVar
    else
      putMVar outMVar EndOfStream
