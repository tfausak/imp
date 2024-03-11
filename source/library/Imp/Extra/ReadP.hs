module Imp.Extra.ReadP where

import qualified Data.List as List
import qualified Text.ParserCombinators.ReadP as ReadP

run :: ReadP.ReadP a -> String -> Maybe a
run x = fmap fst . List.find (null . snd) . ReadP.readP_to_S x
