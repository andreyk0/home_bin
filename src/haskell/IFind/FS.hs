module IFind.FS (
  TextFilePath,
  findAllFilePaths
) where

import Control.Applicative
import Control.DeepSeq
import System.FilePath.Find

import qualified Data.Text as T
import qualified Text.Regex.TDFA as RT
import qualified Text.Regex.TDFA.String as RS

import IFind.Config
import IFind.Opts
import Util.List

-- Convert Strings to Texts and work with that for incremental search
type TextFilePath = T.Text

findAllFilePaths:: IFindOpts -> IO [TextFilePath]
findAllFilePaths opts = do
  filters <- readConfFile opts
  let dF = (filterOut $ excludeDirectories filters) <$> filePath
  let fF = (filterOut $ excludePaths filters) <$> filePath
  let regularFilesOrSymlinks = (fileType ==? RegularFile) ||? (fileType ==? SymbolicLink)
  (fmap (T.pack)) <$> find (() `deepseq` dF) (() `deepseq` fF &&? regularFilesOrSymlinks) (inDir opts)


-- | filter out file paths matching any of the given (as string) regexes
filterOut:: [String] -> FilePath -> Bool
filterOut res fp = not $ anyOf toReMatchers fp
  where
    toReMatchers:: [FilePath -> Bool]
    toReMatchers = fmap (RT.matchTest . strToRegex) res

strToRegex:: String -> RS.Regex
strToRegex s = case RS.compile RT.blankCompOpt RT.blankExecOpt s of
  Left err -> error $ "Unable to compile [" ++ s ++ "] to regex: " ++ err
  Right r -> r
