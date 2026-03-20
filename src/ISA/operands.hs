module ISA.Operands where

import Data.Int (Int16)
import Data.Word (Word16)
import Data.Bits

import ISA.Registers

data Operand
  = Immediate Int16        -- ^ Immediate value
  | Direct Register -- ^ Direct register access
  | Indirect Register -- ^ Indirect register access (memory address in register)
  | Indexed Register Int16 -- ^ Base register plus offset
  deriving (Show, Eq)

operandToBits :: Operand -> [Word16]
operandToBits (Immediate val) = [fromIntegral val .&. 0xFFFF]
operandToBits (Direct _) = []
operandToBits (Indirect _) = []
operandToBits (Indexed _ offset) = [fromIntegral offset .&. 0xFFFF]

operandFieldToBits :: Operand -> Word16
operandFieldToBits (Immediate _) = 0b00
operandFieldToBits (Direct reg) = (0b01 `shiftL` 4) .|. (fromIntegral (registerToInt reg) .&. 0xF)
operandFieldToBits (Indirect reg) = (0b10 `shiftL` 4) .|. (fromIntegral (registerToInt reg) .&. 0xF)
operandFieldToBits (Indexed reg _) = (0b11 `shiftL` 4) .|. (fromIntegral (registerToInt reg) .&. 0xF)

isWritable :: Operand -> Bool
isWritable (Direct _)   = True
isWritable (Indirect _) = True
isWritable (Indexed _ _) = True
isWritable (Immediate _)   = False