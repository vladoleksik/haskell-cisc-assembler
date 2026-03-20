module Parser.SecondPass where

import qualified Data.Map as Map
import Data.Word (Word16)
import ISA.Instructions
import ISA.InstrClasses.B3
import ISA.LabelOffsets
import Parser.SymbolTable
import Parser.Lines

type PassState = (Int, [Instruction]) -- (Current Address, Collected Instructions)

secondPass :: [Line] -> SymbolTable -> [Instruction]
secondPass lines symbolTable = snd $ foldl (processLine symbolTable) (0, []) lines
  where
    processLine :: SymbolTable -> PassState -> Line -> PassState
    processLine table (addr, instructions) line = case (lineNumber line, label line, instruction line) of
        (_, _, Nothing) -> (addr, instructions) -- If there's a label, add it to the table
        (_, label, Just (B3Instr (B3Instruction opc (UnresolvedLabel lbl)))) -> (addr + fromIntegral (sizeofInstruction (B3Instr (B3Instruction opc (UnresolvedLabel lbl)))), instructions ++ [B3Instr (B3Instruction opc (LabelOffset (fromIntegral ((Map.findWithDefault 0 lbl table) - (fromIntegral addr + fromIntegral (sizeofInstruction (B3Instr (B3Instruction opc (UnresolvedLabel lbl)))))))))]) -- If there's an instruction, increment the address by its size
        (_, _, Just instr) -> (addr + fromIntegral (sizeofInstruction instr), instructions ++ [instr]) -- If there's only a normal instruction, just add it to the list of instructions