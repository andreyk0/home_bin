{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (
  main
) where

import Control.Applicative
import Control.Concurrent
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.List
import Data.String.Utils
import System.Console.CmdArgs
import System.Directory

import qualified Codec.Archive.Zip as Z
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Map as Map
import qualified Data.Set as Set


data JarDupsOpts =
  JarDupsOpts { libDir:: FilePath
              , outFile:: Maybe FilePath
              , verbose:: Bool }
    deriving (Show, Data, Typeable)

jarDupsOpts :: JarDupsOpts
jarDupsOpts =
  JarDupsOpts { libDir = "." &= help "path/to/lib/dir/with/jars"
              , outFile = Nothing &= help "output file name, e.g. dups.json"
              , verbose = False &= help "run in a verbose mode"
              } &=
                program "jar-dups" &=
                help "Finds duplicate classes (by FQN) among all of the jars in a given directory"

type ClassName = String

data JarDup =
  JarDup { jars:: [FilePath]
         , classes:: [ClassName] }
    deriving (Show, Data, Typeable)

instance ToJSON JarDup where
     toJSON (JarDup js cs) = object ["jars" .= js, "classes" .= cs]


main:: IO ()
main = do
  opts :: JarDupsOpts <- cmdArgs jarDupsOpts :: IO JarDupsOpts
  cfps <- readAllJars opts
  let dups = findDups cfps
  let jsonTxt = encodePretty $ dups

  case (outFile opts) of
    Just fname -> L8.writeFile fname jsonTxt
    Nothing    -> L8.putStrLn jsonTxt


findDups:: [(ClassName, FilePath)]-> [JarDup]
findDups cfps= fmap (toJarDup) $ Map.toList dupPaths2Cnames
  where
    allClass2Jar = toMap cfps
    className2DupPaths = Map.filter ((1 <) . Set.size) allClass2Jar
    dupPaths2Cnames = Map.fromListWith (++) $ fmap (\(k,v) -> (v, [k])) $ Map.toList className2DupPaths
    toJarDup (js, cns) = JarDup { jars = Set.toList js
                                , classes = sort cns }
    toMap:: [(ClassName, FilePath)] -> Map.Map ClassName (Set.Set FilePath)
    toMap j2ps = Map.fromListWith (Set.union) $ fmap (toCnameAndFPSet) $ j2ps
    toCnameAndFPSet (cn, fp) = (cn, Set.singleton fp)


readAllJars:: JarDupsOpts -> IO [(ClassName, FilePath)]
readAllJars opts = do
  let libDirName = libDir opts
  jarFiles <- filter (endswith ".jar") <$> getDirectoryContents libDirName
  let fullJarPaths = fmap ((libDirName ++ "/") ++) $ jarFiles
  jarContentsMvars <- mapM (readJar opts) fullJarPaths
  concat <$> (mapM (takeMVar) jarContentsMvars)


readJar:: JarDupsOpts -> FilePath -> IO (MVar [(ClassName, FilePath)])
readJar opts jarFname = do
	resMvar <- newEmptyMVar

	_ <- forkIO $ do
		if (verbose opts)
			then putStrLn $ "Reading " ++ (show jarFname) ++ " ..."
			else return ()

		jBytes <- L.readFile jarFname
		let arch = Z.toArchive jBytes
		let jarContents = Z.filesInArchive arch
		let cNames = filter (endswith ".class") jarContents
		putMVar resMvar $ zip cNames $ repeat jarFname

	return resMvar
