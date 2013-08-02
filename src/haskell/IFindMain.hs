module Main (
  main
) where

import IFind.Opts
import IFind.UI
import System.Console.CmdArgs

iFindOpts :: IFindOpts
iFindOpts =
  IFindOpts { inDir = "."             &= help "directory to search recursively"
            , outFile = Nothing       &= help "output file name"
            , searchRe = ""           &= help "initial value of search regex"
            , noDefaultConfig = False &= help "ignore default config settings"
            } &=
              program "ifind" &=
              help ("Interactive 'find' utility, search file names with regexes. " ++
                    " Search text takes multiple regexes, separated by ! which has" ++
                    " an effect similar to \".. |grep -v ..\"." ++
                    " $HOME/.ifind contains default config (default exclusion rules).")

main :: IO ()
main = do
  opts <- cmdArgs iFindOpts
  fps <- runUI opts
  case outFile opts of
    Just fname -> writeFile fname $ unlines fps
    Nothing -> putStrLn $ unlines fps -- not terribly useful but for debugging
