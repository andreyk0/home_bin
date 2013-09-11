--
-- | Host name related utils
--
module MSSH.HostNames(
  expandHostNames,
  padToMaxHostNameLen
) where

import Data.Maybe
import Text.Regex.TDFA


-- | Pad host name strings so that the output column aligns
padToMaxHostNameLen:: [String] -- ^ all host names
                   -> String -- ^ host name to pad
                   -> String
padToMaxHostNameLen hs h =
    h ++ (take (maxHostNameLen - length h) $ repeat ' ')
  where
    maxHostNameLen = maximum $ map (length) hs

-- | Parse host name shortcuts and expand them to full host names.
--   E.g. foo1,2,3:5,11 --> foo1 foo2 foo3 foo4 foo5 foo11
expandHostNames:: String -> [String]
expandHostNames sn =
  case listToMaybe $ splitBaseNameAndNumRanges sn of
    Just (_:baseName:numRanges:suffix:[]) ->
      map (flip (++) suffix) $ map ((++) baseName) $ concat $ map (expandNumRange) $ splitNumRanges numRanges
    Just _ -> [sn]
    Nothing -> [sn]

-- ghci> "foo2bar4x1,2,3:5,11.bar.baz" =~ "^([a-zA-Z0-9\\-_]+[a-zA-Z\\-_])([0-9,:]+)(\\..+)?" :: [[String]]
-- [["foo2bar4x1,2,3:5,11.bar.baz","foo2bar4x","1,2,3:5,11",".bar.baz"]]
splitBaseNameAndNumRanges:: String -> [[String]]
splitBaseNameAndNumRanges s = s =~ "^([a-zA-Z0-9\\-_]+[a-zA-Z\\-_])([0-9,:]+)(\\..+)?"

-- ghci> "1,2:3,4,5" =~ "[^,]+" :: [[String]]
-- [["1"],["2:3"],["4"],["5"]]
splitNumRanges:: String -> [String]
splitNumRanges nr = map (head) (nr =~ "[^,]+" :: [[String]])

-- 1:3 -> [1,2,3]
-- 13 -> [13]
expandNumRange:: String -> [String]
expandNumRange nr = case nums of
    from:to:[]      -> map (show) [ from .. to ]
    from:to:step:[] -> map (show) [ from, from+step .. to ]
    _ -> [nr]
  where nums = map (read . head) (nr =~ "[^:]+" :: [[String]]) :: [Int]
