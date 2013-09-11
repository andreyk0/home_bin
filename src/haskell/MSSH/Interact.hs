module MSSH.Interact(
  haskelineInteract
) where

import Control.Monad.Trans
import System.Console.Haskeline
import System.Directory
import System.IO

-- | Read input lines and send them to STDIN of all SSH processes
haskelineInteract:: String -- ^ prompt text
                 -> [Handle] -- ^ STDIN handles of SSH processes
                 -> IO ()
haskelineInteract prompt stdinHs = do
    haskelineSettings <- fmap (\hf -> defaultSettings { historyFile = Just hf }) msshHistoryFile
    runInputT haskelineSettings$ withInterrupt $ loop prompt stdinHs

-- | Main haskeline loop
loop:: String -> [Handle] -> InputT IO ()
loop prompt hsIn = do
  minput <- handle (\Interrupt -> return Nothing)
                   $ getInputLine prompt
  case minput of
    Nothing -> do
      liftIO $ mapM_ hClose hsIn
    Just l -> do
      _ <- liftIO $ sendInputLine hsIn l
      loop prompt hsIn

-- | Send input line to multiple STDIN handlers
sendInputLine:: [Handle] -> String -> IO ()
sendInputLine hsIn line = do
  _ <- mapM_ ((flip hPutStrLn) line) hsIn
  mapM_ (hFlush) hsIn

-- | Path to command history file
msshHistoryFile:: IO FilePath
msshHistoryFile = do
  hDir <- getHomeDirectory
  return $ hDir ++ "/.mssh"
