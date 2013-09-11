{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (
  main
) where

import Control.Concurrent
import MSSH.HostNames
import MSSH.Interact
import System.Console.CmdArgs
import System.IO
import System.Process

import qualified Control.Concurrent.MSemN as Sem
import qualified Data.Traversable as T

import MSSH.IO
import Util.AnsiColor

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


-- | Starts SSH and output printing IO threads
startSSH:: MSSHOpts -- ^ cmd line opts
        -> MVar OutLine -- ^ write lines of output here
        -> String -- ^ host name to connect to
        -> IO (Handle, ProcessHandle) -- ^ file handle to write commands to (input),
                                      --  process handle (to wait for process to stop)
startSSH opts outLineMV host = do
  (hIn,hOut,hErr,hProc) <- runInteractiveCommand $ (sshCommand opts) ++ " " ++ host
  hSetBuffering hIn LineBuffering
  hSetBuffering hOut LineBuffering
  hSetBuffering hErr LineBuffering
  _ <- forkIO $ consumeStdOut host hOut outLineMV
  _ <- forkIO $ consumeStdErr host hErr outLineMV
  return (hIn, hProc)

msshPrompt:: String
msshPrompt = "mssh> "

main :: IO ()
main = do
    opts <- cmdArgs mSSHOpts

    saveFileFh <- T.forM (outFile opts) $ \fp -> do
      fh <- openFile fp WriteMode
      hSetBuffering fh LineBuffering
      return fh

    let allHostNames = concat $ map (expandHostNames) $ hosts opts
    let consoleColors = if (color opts) then (Cyan, Red) else (Disabled, Disabled)

    sshOutputMVar <- newEmptyMVar
    endOfOutputSem <- Sem.new 0 -- in the end this should go up to num hosts

    let startSSHForHost = startSSH opts sshOutputMVar

    inputAndProcH <- mapM (startSSHForHost) allHostNames
    let hsIn = map (fst) inputAndProcH
    let hsProc = map (snd) inputAndProcH

    let padHostname = padToMaxHostNameLen allHostNames

    _ <- forkIO $ tee msshPrompt consoleColors (padHostname) sshOutputMVar endOfOutputSem saveFileFh

    haskelineInteract msshPrompt hsIn

    mapM_ waitForProcess hsProc -- wait for all procs to stop

    let numSSHStreams = 2 * (fromIntegral . length) allHostNames -- 2 streams (stdin/out) per host

    Sem.wait endOfOutputSem numSSHStreams -- wait for SSH all output to stop
    signalExit sshOutputMVar -- stop the console printing loop
    Sem.wait endOfOutputSem 1 -- 1, loop itself exited

    _ <- T.forM saveFileFh hClose -- finish output to save file

    return ()
