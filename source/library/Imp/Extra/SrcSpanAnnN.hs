module Imp.Extra.SrcSpanAnnN where

import qualified Data.Function as Function
import qualified GHC.Hs as Hs
import qualified GHC.Plugins as Plugin

leftmostSmallest :: Hs.SrcSpanAnnN -> Hs.SrcSpanAnnN -> Hs.SrcSpanAnnN
leftmostSmallest x y = case Function.on Plugin.leftmost_smallest Hs.locA x y of
  LT -> x
  EQ -> x
  GT -> y
