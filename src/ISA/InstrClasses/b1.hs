module ISA.InstrClasses.B1 where

import Data.Bits
import Data.Word (Word16)

import ISA.Operands

data B1Instruction = B1Instruction B1Opcode Operand Operand
  deriving (Show, Eq)

b1InstrToBits :: B1Instruction -> Word16
b1InstrToBits (B1Instruction opcode dstOperand srcOperand) = (fromIntegral (operandFieldToBits dstOperand) .&. 0x3F) .|. ((fromIntegral (operandFieldToBits srcOperand) .&. 0x3F) `shiftL` 6) .|. ((fromIntegral . b1OpcodeToBits) opcode `shiftL` 12)

data B1Opcode = 
    MOV
    | ADD
    | SUB
    | CMP
    | AND
    | OR
    | XOR
    deriving (Show, Eq, Enum)

b1OpcodeToBits :: B1Opcode -> Int
b1OpcodeToBits = fromEnum

mutates :: B1Opcode -> Bool
mutates (CMP) = False
mutates _ = True

b1OpcodeFromString :: String -> Maybe B1Opcode
b1OpcodeFromString str = case str of
    "MOV" -> Just MOV
    "ADD" -> Just ADD
    "SUB" -> Just SUB
    "CMP" -> Just CMP
    "AND" -> Just AND
    "OR"  -> Just OR
    "XOR" -> Just XOR
    _     -> Nothing