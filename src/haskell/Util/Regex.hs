module Util.Regex (
  matchRe
) where

import qualified Text.Regex.TDFA.String as RS

matchRe:: RS.Regex -> FilePath -> Bool
matchRe re fp = case RS.execute re fp of
  Right (Just _) -> True
  _ -> False
