{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import System.Console.CmdArgs

import qualified Data.Attoparsec.Lazy as L hiding (take)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

data PrettyJsonOpts =
  PrettyJsonOpts { file:: FilePath
                 , lenient:: Bool }
    deriving (Show, Data, Typeable)

prettyJsonOpts :: PrettyJsonOpts
prettyJsonOpts =
  PrettyJsonOpts { file = "-" &= help "path/to/file.json"
                 , lenient = False &= help "skip input on parser errors"
                 } &=
                   program "pretty-json" &=
                   help "Extracts and pretty prints JSON from input files or STDIN (default)"

main:: IO ()
main = do
  opts :: PrettyJsonOpts <- cmdArgs prettyJsonOpts :: IO PrettyJsonOpts
  j <- readJsonBytes opts
  printPretty j


printPretty:: Value -> IO ()
printPretty jsonV = do
  let prettyConfig = defConfig { confIndent = 2
                               , confCompare = compare }
  L8.putStrLn $ encodePretty' prettyConfig jsonV


readJsonBytes:: PrettyJsonOpts -> IO Value
readJsonBytes opts = do
  jsonBytes <- case file opts of
    "-" -> L.getContents
    f@_ -> L.readFile f
  case decodeJsonBytes opts jsonBytes of
    [v]  -> return v
    vs@_ -> return $ toJSON vs

decodeJsonBytes :: PrettyJsonOpts -> L.ByteString -> [Value]
decodeJsonBytes opts s = decodeJsonBytes' [] s
  where isLenient = lenient opts
        decodeJsonBytes' acc s' = case L.parse json' s' of
          L.Done ss  v -> if isLenient
                            then decodeJsonBytes' (v:acc) ss
                            else [v]

          L.Fail moreInput contexts errMsg -> if isLenient
                                                then continueParsing acc moreInput
                                                else return $ object [ "parser_error" .= errMsg
                                                                     , "around" .= (L.take 16 moreInput)
                                                                     , "contexts" .= toJSON contexts ]

        continueParsing acc s' = if L.null s'
                                  then reverse acc
                                  else decodeJsonBytes' acc $ L.drop 1 s' -- skip 1 byte and try again
