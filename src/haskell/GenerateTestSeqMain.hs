{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (
  main
) where

import Control.Concurrent
import System.Console.CmdArgs
import Text.Printf

import qualified Data.Traversable as T

data GTSOpts =
  GTSOpts { pattern:: String
          , startFrom:: Integer
          , throttle:: Maybe Int
          , numIncrements:: Integer
          }
          deriving (Show, Data, Typeable)

gtsOpts :: GTSOpts
gtsOpts =
  GTSOpts { pattern = "test-%09d" &= help "printf-like pattern that takes one Integer"
          , startFrom = 0 &= help "initial value of the sequence, defaults to 0"
          , throttle = Nothing &= help "sleep this many milliseconds between increments"
          , numIncrements = 1 &= help "how many lines of output to produce"
          } &=
          program "generate-test-seq" &=
            summary "Generates test data sequence for testing queues, etc"

main :: IO ()
main = do
  opts <- cmdArgs gtsOpts
  loop opts (startFrom opts) $ (numIncrements opts) -1

loop:: GTSOpts -> Integer -> Integer -> IO ()
loop opts i n = do
  putStrLn $ printf (pattern opts) i
  _ <- T.forM (throttle opts) $ do \millis -> threadDelay (millis * 1000)
  if (n>0)
    then loop opts (i+1) (n-1)
    else return ()

