{-# LANGUAGE TemplateHaskell #-}

module IFind.Config (
  FindFilters(..),
  readConfFile
) where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.TH
import System.Directory
import System.Exit (exitFailure)

import qualified Data.ByteString.Lazy as L

import IFind.Opts


-- | regex filters we apply to recursive find
--   initially read from $HOME/.iconfig
data FindFilters = FindFilters { excludeDirectories:: [String]
                               , excludePaths:: [String]
                               } deriving (Show)

$(deriveJSON id ''FindFilters)


emptyFilters:: FindFilters
emptyFilters =
  FindFilters { excludeDirectories = []
              , excludePaths = []
              }


defaultFilters:: FindFilters
defaultFilters =
  FindFilters { excludeDirectories = ["\\.git$"
                                     , "\\.svn$"
                                     ]
              , excludePaths = [ "^.*\\.class$"
                               , "^.*\\.swp$"
                               , "^.*\\.hi$"
                               , "^.*\\.o$"
                               , "^.*\\.jar$"
                               ]
              }


confFileName:: IO FilePath
confFileName = do
  homeDir <- getHomeDirectory
  return $ homeDir ++ "/.ifind"


createDefaultConfFile:: IO ()
createDefaultConfFile = do
  fName <- confFileName
  let jsonTxt = encodePretty $ defaultFilters
  L.writeFile fName jsonTxt


readConfFile:: IFindOpts -> IO FindFilters
readConfFile opts = do
  fName <- confFileName
  foundConfFile <- doesFileExist fName
  _ <- if not foundConfFile then createDefaultConfFile else return ()

  r <- if noDefaultConfig opts
         then return $ Right emptyFilters
         else eitherDecode' <$> L.readFile fName

  case r of
    Left e -> putStrLn e >> exitFailure
    Right ff -> return ff
