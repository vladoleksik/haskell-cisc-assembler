module ISA.InstrClasses.B4 where

import Data.Bits
import Data.Word (Word16)

data B4Instruction = B4Instruction B4Opcode
  deriving (Show, Eq)

b4InstrToBits :: B4Instruction -> Word16
b4InstrToBits (B4Instruction opcode) = (fromIntegral . b4OpcodeToBits) opcode .|. (0b111 `shiftL` 13) -- convert to bits and or with 0b111<<12 for the class

data B4Opcode = 
    CLC
    | CLV
    | CLZ
    | CLS
    | CCC
    | SEC
    | SEV
    | SEZ
    | SES
    | SCC
    | NOP
    | RET
    | RETI
    | HALT
    | WAIT
    | PUSH_PC
    | POP_PC
    | PUSH_FLAG
    | POP_FLAG
    deriving (Show, Eq, Enum)

b4OpcodeToBits :: B4Opcode -> Int
b4OpcodeToBits = fromEnum

b4OpcodeFromString :: String -> Maybe B4Opcode
b4OpcodeFromString str = case str of
    "CLC" -> Just CLC
    "CLV" -> Just CLV
    "CLZ" -> Just CLZ
    "CLS" -> Just CLS
    "CCC" -> Just CCC
    "SEC" -> Just SEC
    "SEV" -> Just SEV
    "SEZ" -> Just SEZ
    "SES" -> Just SES
    "SCC" -> Just SCC
    "NOP" -> Just NOP
    "RET" -> Just RET
    "RETI" -> Just RETI
    "HALT" -> Just HALT
    "WAIT" -> Just WAIT
    "PUSH_PC" -> Just PUSH_PC
    "POP_PC" -> Just POP_PC
    "PUSH_FLAG" -> Just PUSH_FLAG
    "POP_FLAG" -> Just POP_FLAG
    _     -> Nothing