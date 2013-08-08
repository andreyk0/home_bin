module Main (
  main
) where

import IFind.Config
import IFind.Opts
import IFind.UI
import System.Console.CmdArgs

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

iFindOpts :: IFindOpts
iFindOpts =
  IFindOpts { inDir = "."              &= help "directory to search recursively"
            , outFile = Nothing        &= help "output file name"
            , searchRe = ""            &= help "initial value of search regex"
            , noDefaultFilters = False &= help "ignore default directory/path filters found in the config"
            , caseInsensitive  = False &= help "turns on case-insensitive search"
            } &=
              program "ifind" &=
              help ("Interactive 'find' utility, search file names with regexes. " ++
                    " Search text takes multiple regexes, separated by ! which has" ++
                    " an effect similar to \".. |grep -v ..\"." ++
                    " Ctrl-u toggles case (in)sensitive search." ++
                    " $HOME/.ifind contains default config (default exclusion rules).")

main :: IO ()
main = do
  opts <- cmdArgs iFindOpts
  conf <- readConfFile opts
  fps <- runUI opts conf
  case outFile opts of
    Just fname -> TIO.writeFile fname $ T.unlines fps
    Nothing -> TIO.putStr $ T.unlines fps -- not terribly useful but for debugging
