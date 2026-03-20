module ISA.InstrClasses.B3 where

import Data.Bits
import Data.Word (Word16)

import ISA.LabelOffsets

data B3Instruction = B3Instruction B3Opcode LabelOffset
  deriving (Show, Eq)

b3InstrToBits :: B3Instruction -> Word16
b3InstrToBits (B3Instruction opcode labelOffset) = (fromIntegral (labelOffsetToBits labelOffset) .&. 0xFF) .|. ((fromIntegral . b3OpcodeToBits) opcode `shiftL` 8) .|. (0b11 `shiftL` 14)

data B3Opcode = 
    BR
    | BNE
    | BEQ
    | BPL
    | BMI
    | BCS
    | BCC
    | BVS
    | BVC
    deriving (Show, Eq, Enum)

b3OpcodeToBits :: B3Opcode -> Int
b3OpcodeToBits = fromEnum

b3OpcodeFromString :: String -> Maybe B3Opcode
b3OpcodeFromString str = case str of
    "BR" -> Just BR
    "BNE" -> Just BNE
    "BEQ" -> Just BEQ
    "BPL" -> Just BPL
    "BMI" -> Just BMI
    "BCS" -> Just BCS
    "BCC" -> Just BCC
    "BVS" -> Just BVS
    "BVC" -> Just BVC
    _ -> Nothing