module ISA.Instructions where

import Data.Word (Word16)

import ISA.InstrClasses.B1
import ISA.InstrClasses.B2
import ISA.InstrClasses.B3
import ISA.InstrClasses.B4
import ISA.Operands

data Instruction = 
    B1Instr B1Instruction
  | B2Instr B2Instruction
  | B3Instr B3Instruction
  | B4Instr B4Instruction
  deriving (Show, Eq)

instructionToBits :: Instruction -> [Word16]
instructionToBits (B1Instr instr) =
    [b1InstrToBits instr] ++ case instr of
        B1Instruction _ opd ops -> operandToBits ops ++ operandToBits opd
instructionToBits (B2Instr instr) = 
    [b2InstrToBits instr] ++ case instr of
        B2Instruction _ opd -> operandToBits opd
instructionToBits (B3Instr instr) = [b3InstrToBits instr]
instructionToBits (B4Instr instr) = [b4InstrToBits instr]

validInstruction :: Instruction -> Bool
validInstruction (B1Instr (B1Instruction opcode opd _)) =
    (not . ISA.InstrClasses.B1.mutates) opcode || isWritable opd
validInstruction (B2Instr (B2Instruction opcode opd)) =
    (not . ISA.InstrClasses.B2.mutates) opcode || isWritable opd
validInstruction (B3Instr _) = True
validInstruction (B4Instr _) = True

-- size of instruction in Bytes is 2 * size of list returned by instructionToBits
sizeofInstruction :: Instruction -> Int
sizeofInstruction instr = 2 * length (instructionToBits instr)