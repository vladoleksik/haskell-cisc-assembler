module Parser.FirstPass where

import qualified Data.Map as Map
import Data.Word (Word16)
import ISA.Instructions
import Parser.SymbolTable
import Parser.Lines

type PassState = (Int, SymbolTable) -- (Current Address, Collected Labels)

firstPass :: [Line] -> SymbolTable
firstPass lines = snd $ foldl processLine (0, Map.empty) lines
  where
    processLine :: PassState -> Line -> PassState
    processLine (addr, table) line = case (lineNumber line, label line, instruction line) of
        (_, Just lbl, Nothing) -> (addr, Map.insert lbl (fromIntegral addr) table) -- If there's a label, add it to the table
        (_, Nothing, Just instr) -> (addr + fromIntegral (sizeofInstruction instr), table) -- If there's an instruction, increment the address by its size
        (_, Just lbl, Just instr) -> (addr + fromIntegral (sizeofInstruction instr), Map.insert lbl (fromIntegral addr) table) -- If there's both a label and an instruction, add the label and increment the address
        (_, Nothing, Nothing) -> (addr, table) -- If there's neither, just continue with the same address