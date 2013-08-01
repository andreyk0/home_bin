module IFind.FS (
  findAllFilePaths
) where

import Control.Applicative
import System.FilePath.Find
import Text.Regex.Posix ((=~))

import IFind.Config
import IFind.Opts

findAllFilePaths:: IFindOpts -> IO [FilePath]
findAllFilePaths opts = do
  filters <- readConfFile opts
  let filterNot res fp = not $ or $ fmap (fp =~) res
  let dF = filterNot (excludeDirectories filters) <$> filePath
  let fF = filterNot (excludePaths filters) <$> filePath
  let regularFilesOrSymlinks = (fileType ==? RegularFile) ||? (fileType ==? SymbolicLink)
  find (dF) (fF &&? regularFilesOrSymlinks) (inDir opts)
