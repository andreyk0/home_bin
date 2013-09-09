{-# LANGUAGE TemplateHaskell #-}

module IFind.Config (
  IFindConfig(..),
  FindFilters(..),
  UIColors(..),
  readConfFile
) where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.TH
import Graphics.Vty.Attributes
import System.Directory
import System.Exit (exitFailure)

import qualified Data.ByteString.Lazy as L

import IFind.Opts

instance FromJSON Color where
  parseJSON = fmap ISOColor . parseJSON

instance ToJSON Color where
  toJSON (ISOColor c) = toJSON c
  toJSON (Color240 _) = error "Expected ISOColor, got Color240"

data UIColors =
  UIColors { helpColorDescription :: [String]
           , focusedItemForegroundColor:: Color
           , focusedItemBackgroundColor:: Color
           , searchCountForegroundColor:: Color
           , searchCountBackgroundColor:: Color
           } deriving (Show)

$(deriveJSON defaultOptions ''UIColors)

-- | regex filters we apply to recursive find
data FindFilters = FindFilters { excludeDirectories:: [String]
                               , excludePaths:: [String]
                               } deriving (Show)

$(deriveJSON defaultOptions ''FindFilters)

-- | Common config parameters
--   initially read from $HOME/.iconfig
data IFindConfig =
  IFindConfig { uiColors:: UIColors
              , findFilters:: FindFilters
              } deriving (Show)

$(deriveJSON defaultOptions ''IFindConfig)


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
              , excludePaths = [ "\\.class$"
                               , "\\.swp$"
                               , "\\.hi$"
                               , "\\.o$"
                               , "\\.jar$"
                               ]
              }

defaultUIColors:: UIColors
defaultUIColors =
  UIColors { helpColorDescription = [
               "Color mappings:",
               "black         =  0",
               "red           =  1",
               "green         =  2",
               "yellow        =  3",
               "blue          =  4",
               "magenta       =  5",
               "cyan          =  6",
               "white         =  7",
               "bright_black  =  8",
               "bright_red    =  9",
               "bright_green  = 10",
               "bright_yellow = 11",
               "bright_blue   = 12",
               "bright_magenta= 13",
               "bright_cyan   = 14",
               "bright_white  = 15"
             ]
           , focusedItemForegroundColor = black
           , focusedItemBackgroundColor = white
           , searchCountForegroundColor = black
           , searchCountBackgroundColor = yellow
           }


defaultConfig:: IFindConfig
defaultConfig =
  IFindConfig { uiColors = defaultUIColors
              , findFilters = defaultFilters
              }


confFileName:: IO FilePath
confFileName = do
  homeDir <- getHomeDirectory
  return $ homeDir ++ "/.ifind"


createDefaultConfFile:: IO ()
createDefaultConfFile = do
  fName <- confFileName
  let jsonTxt = encodePretty $ defaultConfig
  L.writeFile fName jsonTxt


readConfFile:: IFindOpts -> IO IFindConfig
readConfFile opts = do
  fName <- confFileName
  foundConfFile <- doesFileExist fName
  _ <- if not foundConfFile then createDefaultConfFile else return ()

  r <- if noDefaultFilters opts
         then return $ Right defaultConfig { findFilters = emptyFilters }
         else eitherDecode' <$> L.readFile fName

  case r of
    Left e -> putStrLn e >> exitFailure
    Right ff -> return ff
