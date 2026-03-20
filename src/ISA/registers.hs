module ISA.Registers where

data Register = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  deriving (Show, Eq, Enum)

-- | Convert a register to its integer index (R0 -> 0, R1 -> 1, ...)
registerToInt :: Register -> Int
registerToInt = fromEnum

registerFromInt :: Int -> Maybe Register
registerFromInt n
  | n >= 0 && n <= 15 = Just (toEnum n)
  | otherwise = Nothing