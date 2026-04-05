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
b4OpcodeToBits op = case op of
    CLC -> 0x00F7
    CLV -> 0x00FE
    CLZ -> 0x00FB
    CLS -> 0x00FD
    CCC -> 0x00F0
    SEC -> 0x0108
    SEV -> 0x0101
    SEZ -> 0x0104
    SES -> 0x0102
    SCC -> 0x010F
    NOP -> 0x0200
    RET -> 0x0A00
    RETI -> 0x0C00
    HALT -> 0x0300
    WAIT -> 0x0B00
    PUSH_PC -> 0x0600
    POP_PC -> 0x0700
    PUSH_FLAG -> 0x0800
    POP_FLAG -> 0x0900

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