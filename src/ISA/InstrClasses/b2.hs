module ISA.InstrClasses.B2 where

import Data.Bits
import Data.Word (Word16)

import ISA.Operands

data B2Instruction = B2Instruction B2Opcode Operand
  deriving (Show, Eq)

b2InstrToBits :: B2Instruction -> Word16
b2InstrToBits (B2Instruction opcode operand) = (fromIntegral (operandFieldToBits operand) .&. 0x3F) .|. ((fromIntegral . b2OpcodeToBits) opcode `shiftL` 6) .|. (0b1 `shiftL` 15) -- convert to bits and or with 0b1<<15 for the class

data B2Opcode = 
    CLR
    | NEG
    | INC
    | DEC
    | ASL
    | ASR
    | LSR
    | ROL
    | ROR
    | RLC
    | RRC
    | JMP
    | CALL
    | PUSH
    | POP
    deriving (Show, Eq, Enum)

b2OpcodeToBits :: B2Opcode -> Int
b2OpcodeToBits = fromEnum

mutates :: B2Opcode -> Bool
mutates (JMP) = False
mutates (CALL) = False
mutates (PUSH) = False
mutates _ = True

b2OpcodeFromString :: String -> Maybe B2Opcode
b2OpcodeFromString str = case str of
    "CLR" -> Just CLR
    "NEG" -> Just NEG
    "INC" -> Just INC
    "DEC" -> Just DEC
    "ASL" -> Just ASL
    "ASR" -> Just ASR
    "LSR" -> Just LSR
    "ROL" -> Just ROL
    "ROR" -> Just ROR
    "RLC" -> Just RLC
    "RRC" -> Just RRC
    "JMP" -> Just JMP
    "CALL" -> Just CALL
    "PUSH" -> Just PUSH
    "POP" -> Just POP
    _     -> Nothing