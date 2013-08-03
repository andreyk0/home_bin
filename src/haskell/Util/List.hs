{- | Simple list utility functions -}
module Util.List (
  anyOf
) where

-- | Couldn't find it on hoogle
anyOf:: [a -> Bool] -> a -> Bool
anyOf fs x = case fs of
               []    -> False
               f:fs' -> f x || anyOf fs' x
