{-# LANGUAGE OverloadedStrings #-}

module IFind.UI (
  runUI
) where

import Control.Applicative
import Data.Either
import Data.IORef
import Graphics.Vty hiding (pad)
import Graphics.Vty.Widgets.All
import System.Exit (exitSuccess)
import Text.Printf

import qualified Text.Regex.TDFA as RT
import qualified Text.Regex.TDFA.String as RS

import qualified Data.Text as T

import IFind.FS
import IFind.Opts
import Util.List
import Util.Regex


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
              , allFilePaths:: [FilePath]
              , ignoreCase:: IORef Bool
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
  matchingFilePathsRef <- newIORef allFilePaths'
  ignoreCaseRef <- newIORef $ caseInsensitive opts

  let sApp = SearchApp { uiWidget = uiWidget'
                       , statusWidget = statusWidget'
                       , editSearchWidget = editSearchWidget'
                       , searchResultsWidget = searchResultsWidget'
                       , activateHandlers = activateHandlers'
                       , matchingFilePaths = matchingFilePathsRef
                       , allFilePaths = allFilePaths'
                       , ignoreCase = ignoreCaseRef
                       }

  _ <- updateSearchResults sApp

  editSearchWidget' `onActivate` \_ -> do
    shutdownUi

  editSearchWidget' `onChange` \_ -> do
    updateSearchResults sApp
    return ()

  editSearchWidget' `onKeyPressed` \_ key mods -> do
    case (key, mods) of
      (KASCII 'u', [MCtrl]) -> do
        ic <- readIORef ignoreCaseRef
        writeIORef ignoreCaseRef (not ic)
        updateSearchResults sApp
        return True
      (_, _)     ->
        return False

  fg <- newFocusGroup

  fg `onKeyPressed` \_ key _ -> do
    case key of
      KEsc -> do
        shutdownUi
        exitSuccess
      _    -> return False

  _ <- addToFocusGroup fg editSearchWidget'

  return (sApp, fg)


updateSearchResults:: SearchApp -> IO ()
updateSearchResults sApp = do
  clearList $ searchResultsWidget sApp
  searchEditTxt <- getEditText $ editSearchWidget sApp
  ignoreCase' <- readIORef $ ignoreCase sApp

  case searchTxtToFilterPredicate ignoreCase' searchEditTxt of
    Left es -> do
      writeIORef (matchingFilePaths sApp) []
      addToResultsList sApp es

    Right filterPredicate -> do
      let matchingFps = filter (filterPredicate) $ allFilePaths sApp

      -- height of the screen, don't need to add to list more results than this
      maxHeight <- fromIntegral <$> region_height  <$> (terminal_handle >>= display_bounds)

      writeIORef (matchingFilePaths sApp) matchingFps
      addToResultsList sApp $ take maxHeight matchingFps

  updateStatusText sApp


addToResultsList:: SearchApp -> [String] -> IO ()
addToResultsList sApp xs =
  mapM_ (\x -> do
          xw <- plainText x
          addToList (searchResultsWidget sApp) x xw) $ fmap (T.pack) xs


updateStatusText:: SearchApp -> IO ()
updateStatusText sApp = do
  matchingFps <- readIORef (matchingFilePaths sApp)
  let numResults = length matchingFps
  searchEditTxt <- getEditText $ editSearchWidget sApp
  ignoreCase' <- readIORef $ ignoreCase sApp
  let statusChr = if ignoreCase' then '*' else ']'

  if T.null searchEditTxt
    then setText (statusWidget sApp) $ T.pack $ "Search: "
    else setText (statusWidget sApp) $ T.pack $ printf "[%5d%c " numResults statusChr


-- | searchTxt can be of the form "foo!bar!baz",
--   this behaves similar to 'grep foo | grep -v bar | grep -v baz'
--   Return either Left regex compilation errors or Right file path testing predicate
searchTxtToFilterPredicate:: Bool -> T.Text -> Either [String] (FilePath -> Bool)
searchTxtToFilterPredicate reIgnoreCase searchEditTxt =
  case partitionEithers compileRes of
    ([], (includeRe:excludeRes)) -> Right $ \fp -> (matchRe includeRe fp) &&
                                                   (not . anyOf (fmap (matchRe) excludeRes) $ fp)
    (errs, _) -> Left errs

  where
    searchTxt = if T.null searchEditTxt then "." else searchEditTxt

    mkRe:: T.Text -> Either String RS.Regex
    mkRe t = RS.compile reCompOpt reExecOpt $ T.unpack t

    reCompOpt = RT.CompOption { RT.caseSensitive = not reIgnoreCase
                              , RT.multiline = False
                              , RT.rightAssoc = False
                              , RT.newSyntax = True
                              , RT.lastStarGreedy = False }

    reExecOpt = RT.ExecOption { RT.captureGroups = False }


    compileRes:: [Either String RS.Regex]
    compileRes = fmap (mkRe) $ T.splitOn "!" searchTxt
