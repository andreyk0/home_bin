{-# LANGUAGE OverloadedStrings #-}

module IFind.UI (
  runUI
) where

import Control.Applicative
import Control.Exception
import Data.IORef
import Graphics.Vty hiding (pad)
import Graphics.Vty.Widgets.All
import System.Exit (exitSuccess)
import Text.Regex.Posix ((=~))

import qualified Data.Text as T

import IFind.FS
import IFind.Opts


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


-- | runs UI, returns matching file paths
runUI :: IFindOpts -> IO [FilePath]
runUI opts = do
  (ui, fg) <- newSearchApp opts

  c <- newCollection
  _ <- addToCollection c (uiWidget ui) fg

  runUi c $ defaultContext { focusAttr = def_attr
                           }

  readIORef $ matchingFilePaths ui



newSearchApp :: IFindOpts -> IO (SearchApp, Widget FocusGroup)
newSearchApp opts = do
  editSearchWidget' <- editWidget
  searchResultsWidget' <- newTextList def_attr []
  footerWidget' <- plainText "-->"
  activateHandlers' <- newHandlers

  _ <- setEditText editSearchWidget' $ T.pack (searchRe opts)

  uiWidget'  <- ((plainText "Search: ") <++> (vFixed 1 editSearchWidget'))
             <-->
             (return searchResultsWidget')
             <-->
             (vFixed 1 footerWidget')

  allFilePaths' <- findAllFilePaths opts
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

  _ <- updateSearchResults sApp

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

  _ <- addToFocusGroup fg editSearchWidget'

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
      setText (footerWidget sApp) $ T.pack $ "#> [" ++ (show numResults) ++ "]"


