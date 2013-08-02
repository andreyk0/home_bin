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
import Text.Printf
import Text.Regex.Posix ((=~))

import qualified Data.Text as T

import IFind.FS
import IFind.Opts


-- This type isn't pretty, but we have to specify the type of the
-- complete interface.  Initially you can let the compiler tell you
-- what it is.
type T = (Box (Box (HFixed FormattedText) (VFixed Edit))
              (List T.Text FormattedText))

data SearchApp =
  SearchApp {  -- widgets
                uiWidget :: Widget T
              , statusWidget :: Widget FormattedText
              , editSearchWidget :: Widget Edit
              , searchResultsWidget :: Widget (List T.Text FormattedText)
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
  statusWidget' <- plainText "*>"
  activateHandlers' <- newHandlers

  _ <- setEditText editSearchWidget' $ T.pack (searchRe opts)

  uiWidget'  <- ((hFixed 8  statusWidget') <++> (vFixed 1 editSearchWidget'))
             <-->
             (return searchResultsWidget')

  allFilePaths' <- findAllFilePaths opts
  allFilePathsRef <- newIORef allFilePaths'
  matchingFilePathsRef <- newIORef allFilePaths'

  let sApp = SearchApp { uiWidget = uiWidget'
                       , statusWidget = statusWidget'
                       , editSearchWidget = editSearchWidget'
                       , searchResultsWidget = searchResultsWidget'
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
  searchEditTxt <- getEditText $ editSearchWidget sApp
  allFilePaths' <- readIORef . allFilePaths $ sApp

  matchingFilePathsEth <- Control.Exception.try $ do
    let matchingFps = filter (searchTxtToFilterPredicate searchEditTxt) allFilePaths'
    _ <- evaluate $ take 1 matchingFps -- force regex eval and exception
    return matchingFps

  -- height of the screen, don't need to add to list more results than this
  maxHeight <- fromIntegral <$> region_height  <$> (terminal_handle >>= display_bounds)

  case matchingFilePathsEth of
    Left (SomeException e) -> do
      writeIORef (matchingFilePaths sApp) []
      let errorTxt = T.pack . show $ e
      errorTxtWidget <- plainText errorTxt
      addToList (searchResultsWidget sApp) errorTxt errorTxtWidget
    Right fps -> do
      writeIORef (matchingFilePaths sApp) fps
      mapM_ (\fp -> do
              fpw <- plainText fp
              addToList (searchResultsWidget sApp) fp fpw) $ fmap (T.pack) $ take maxHeight fps

  updateStatusText sApp


updateStatusText:: SearchApp -> IO ()
updateStatusText sApp = do
  numRsults <- length <$> readIORef (matchingFilePaths sApp)
  searchEditTxt <- getEditText $ editSearchWidget sApp
  if T.null searchEditTxt
    then setText (statusWidget sApp) $ T.pack $ "Search: "
    else setText (statusWidget sApp) $ T.pack $ printf "[%5d] " numRsults


-- | searchTxt can be of the form "foo!bar!baz",
--   this behaves similar to 'grep foo | grep -v bar | grep -v baz'
searchTxtToFilterPredicate:: T.Text -> (FilePath -> Bool)
searchTxtToFilterPredicate searchEditTxt =
  (\fp -> (filterIncludePredicate fp) &&
          (not . filterExcludePredicate $ fp))
  where
    searchTxt = if T.null searchEditTxt then "." else searchEditTxt

    searchInclude:searchExclude = T.splitOn "!" searchTxt

    txtToFilterPredicate reStr x = x =~ (T.unpack reStr)
    matchAnyOfPredicate reStrs x = or $ map (\p -> p x) $ map (txtToFilterPredicate) reStrs

    filterIncludePredicate = txtToFilterPredicate searchInclude
    filterExcludePredicate = matchAnyOfPredicate searchExclude

