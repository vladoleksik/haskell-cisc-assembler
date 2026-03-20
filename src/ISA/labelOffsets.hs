module ISA.LabelOffsets where

import Data.Int (Int8)

data LabelOffset = LabelOffset Int8
  | UnresolvedLabel String
  deriving (Show, Eq)

labelOffsetToBits :: LabelOffset -> Int8
labelOffsetToBits (LabelOffset offset) = offset
labelOffsetToBits (UnresolvedLabel _) = 0