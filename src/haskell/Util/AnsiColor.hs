module Util.AnsiColor (
  AnsiColor(..),
  ansiColor
) where

import Text.Printf

data AnsiColor = Black | Red | Green | Yellow  | Blue  | Magenta | Cyan  | White | Disabled
  deriving (Show, Enum)

-- | Quick color-printing hack
ansiColor:: AnsiColor -> String -> String
ansiColor c s = case c of
  Disabled -> s
  _ -> printf "\x1b[%d;2m%s\x1b[0m" (30+ (fromEnum c)) s
