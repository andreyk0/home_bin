module IFind.FS (
  findAllFilePaths
) where

import Control.Applicative
import Control.DeepSeq
import System.FilePath.Find

import qualified Text.Regex.TDFA as RT
import qualified Text.Regex.TDFA.String as RS

import IFind.Config
import IFind.Opts
import Util.List
import Util.Regex

findAllFilePaths:: IFindOpts -> IO [FilePath]
findAllFilePaths opts = do
  filters <- readConfFile opts
  let dF = (filterOut $ excludeDirectories filters) <$> filePath
  let fF = (filterOut $ excludePaths filters) <$> filePath
  let regularFilesOrSymlinks = (fileType ==? RegularFile) ||? (fileType ==? SymbolicLink)
  find (() `deepseq` dF) (() `deepseq` fF &&? regularFilesOrSymlinks) (inDir opts)


-- | filter out file paths matching any of the given (as string) regexes
filterOut:: [String] -> FilePath -> Bool
filterOut res fp = not $ anyOf toReMatchers fp
  where
    toReMatchers:: [FilePath -> Bool]
    toReMatchers = fmap (matchRe . strToRegex) res

strToRegex:: String -> RS.Regex
strToRegex s = case RS.compile RT.blankCompOpt RT.blankExecOpt s of
  Left err -> error $ "Unable to compile [" ++ s ++ "] to regex: " ++ err
  Right r -> r
