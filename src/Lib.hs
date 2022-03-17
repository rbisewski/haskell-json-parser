--
-- Misc functions
--
module Lib
    ( splitByChar
    ) where

splitByChar :: (Char -> Bool) -> String -> [String]
splitByChar p s = case dropWhile p s of
                      "" -> []
                      s' -> w : splitByChar p s''
                            where (w, s'') = break p s'
