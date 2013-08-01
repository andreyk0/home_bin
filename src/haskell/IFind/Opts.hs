{-# LANGUAGE DeriveDataTypeable #-}

module IFind.Opts (
  IFindOpts(..)
) where

import System.Console.CmdArgs

data IFindOpts =
  IFindOpts { inDir :: FilePath
            , outFile:: Maybe FilePath
            , searchRe:: String
            , noDefaultConfig:: Bool }
    deriving (Show, Data, Typeable)
