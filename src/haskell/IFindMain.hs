{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative
import Control.DeepSeq (deepseq)
import Control.Exception
import Control.Monad
import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Aeson.TH (deriveJSON)
import Data.IORef
import Graphics.Vty hiding (pad)
import Graphics.Vty.Widgets.All
import System.Directory
import System.Exit (exitSuccess)
import System.FilePath.Find
import System.IO
import Text.Regex.Posix ((=~))

import qualified Data.Text as T

-- This type isn't pretty, but we have to specify the type of the
-- complete interface.  Initially you can let the compiler tell you
-- what it is.
type T = Box (Box (Box FormattedText (VFixed Edit)) (List T.Text FormattedText))
             (VFixed FormattedText)

data SearchApp =
  SearchApp {  -- widgets
                uiWidget :: Widget T
              , editSearchWidget :: Widget Edit
              , searchResultsWidget :: Widget (List T.Text FormattedText)
              , footerWidget :: Widget FormattedText
              , activateHandlers :: Handlers SearchApp
              -- search state
              , matchingFilePaths:: IORef [FilePath]
              , allFilePaths:: IORef [FilePath]
              }


-- | regex filters we apply to recursive find
--   initially read from $HOME/.iconfig
data FindFilters = FindFilters { excludeDirectories:: [String]
                               , excludePaths:: [String]
                               } deriving (Show)

$(deriveJSON id ''FindFilters)

newSearchApp :: FilePath -> IO (SearchApp, Widget FocusGroup)
newSearchApp rootFilePath = do
  editSearchWidget' <- editWidget
  searchResultsWidget' <- newTextList def_attr []
  footerWidget' <- plainText "-->"
  activateHandlers' <- newHandlers

  uiWidget'  <- ((plainText "Search: ") <++> (vFixed 1 editSearchWidget'))
             <-->
             (return searchResultsWidget')
             <-->
             (vFixed 1 footerWidget')

  allFilePaths' <- findAllFilePaths rootFilePath
  allFilePathsRef <- newIORef allFilePaths'
  matchingFilePathsRef <- newIORef allFilePaths'

  let sApp = SearchApp { uiWidget = uiWidget'
                       , editSearchWidget = editSearchWidget'
                       , searchResultsWidget = searchResultsWidget'
                       , footerWidget = footerWidget'
                       , activateHandlers = activateHandlers'
                       , matchingFilePaths = matchingFilePathsRef
                       , allFilePaths = allFilePathsRef
                       }

  editSearchWidget' `onActivate` \_ -> do
    shutdownUi

  editSearchWidget' `onChange` \_ -> do
    updateSearchResults sApp
    return ()

  fg <- newFocusGroup

  fg `onKeyPressed` \_ k _ -> do
    case k of
      KEsc -> exitSuccess
      _    -> return False

  addToFocusGroup fg editSearchWidget'
  --addToFocusGroup fg searchResultsWidget'

  return (sApp, fg)


updateSearchResults:: SearchApp -> IO ()
updateSearchResults sApp = do
  clearList $ searchResultsWidget sApp
  searchTxt <- T.unpack <$> (getEditText $ editSearchWidget sApp)
  allFilePaths' <- readIORef . allFilePaths $ sApp

  matchingFilePathsEth <- Control.Exception.try $ do
    let filterPredicate = (flip (=~)) searchTxt
    let matchingFps = filter (filterPredicate) allFilePaths'
    _ <- evaluate $ take 1 matchingFps -- force regex eval and exception
    return matchingFps

  case matchingFilePathsEth of
    Left (SomeException e) -> do
      writeIORef (matchingFilePaths sApp) []
      setText (footerWidget sApp) (T.pack . show $ e)
    Right fps -> do
      writeIORef (matchingFilePaths sApp) fps
      _ <- mapM_ (\fp -> do
                   fpw <- plainText fp
                   addToList (searchResultsWidget sApp) fp fpw) $ fmap (T.pack) fps
      numResults <- (getListSize . searchResultsWidget) sApp
      setText (footerWidget sApp) $ T.pack $ searchTxt ++ " :> " ++ (show numResults)




findAllFilePaths:: FilePath -> IO [FilePath]
findAllFilePaths r = find (fileName /=? ".git") always r


main :: IO ()
main = do
  (ui, fg) <- newSearchApp "."

  c <- newCollection
  _ <- addToCollection c (uiWidget ui) fg

  runUi c $ defaultContext { focusAttr = def_attr
                           }

  fps <- readIORef $ matchingFilePaths ui
  mapM_ (putStrLn) fps


