{-# LANGUAGE DeriveDataTypeable #-}

module IFind.Opts (
  IFindOpts(..)
) where

import System.Console.CmdArgs

data IFindOpts =
  IFindOpts { inDir :: FilePath
            , outFile:: Maybe FilePath
            , searchRe:: String
            , noDefaultFilters:: Bool
            , caseInsensitive:: Bool }
    deriving (Show, Data, Typeable)
