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
                help "Usage: mssh host1 host2.foo.bar host4,5 host6,7:9 host10:20:2." &=
                details [ "Host names are expanded:",
                          " [foo1,2.your.prod] becomes [foo1.your.prod, foo2.your.prod],",
                          " [foo1:3.your.prod] becomes [foo1.your.prod, foo2.your.prod, foo3.your.prod]",
                          " [foo1:5:2.your.prod] becomes [foo1.your.prod, foo3.your.prod, foo5.your.prod]",
                          "",
                          "To quickly access prod machines via a gateway host add something like this to your ~/.ssh/config",
                          "  Host *.your.prod ",
                          "  ProxyCommand ssh -q -o ... your.gw.com nc %h %p ",
                          "  TCPKeepAlive yes",
                          ""
                        ]


-- | Starts SSH and output printing IO threads
startSSH:: MSSHOpts -- ^ cmd line opts
        -> Tee -- ^ send lines of output here
        -> String -- ^ host name to connect to
        -> IO (Handle, ProcessHandle) -- ^ file handle to write commands to (input),
                                      --  process handle (to wait for process to stop)
startSSH opts tee host = do
  (hIn,hOut,hErr,hProc) <- runInteractiveCommand $ (sshCommand opts) ++ " " ++ host
  hSetBuffering hIn LineBuffering
  hSetBuffering hOut LineBuffering
  hSetBuffering hErr LineBuffering
  _ <- forkIO $ consumeStdOut host hOut tee
  _ <- forkIO $ consumeStdErr host hErr tee
  return (hIn, hProc)

msshPrompt:: String
msshPrompt = "mssh> "

main :: IO ()
main = do
    opts <- cmdArgs mSSHOpts

    let allHostNames = concat $ map (expandHostNames) $ hosts opts
    let consoleColors = if (color opts) then (Cyan, Red) else (Disabled, Disabled)

    let padHostname = padToMaxHostNameLen allHostNames
    tee <- newTee msshPrompt consoleColors (padHostname) (outFile opts)

    let startSSHForHost = startSSH opts tee

    inputAndProcH <- mapM (startSSHForHost) allHostNames
    let hsIn = map (fst) inputAndProcH
    let hsProc = map (snd) inputAndProcH

    _ <- forkIO $ runTee tee

    haskelineInteract msshPrompt hsIn

    mapM_ waitForProcess hsProc -- wait for all procs to stop

    let numSSHStreams = 2 * (fromIntegral . length) allHostNames -- 2 streams (stdin/out) per host

    awaitTermination tee numSSHStreams
