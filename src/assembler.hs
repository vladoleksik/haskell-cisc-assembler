module Assembler (parseFile, buildSymbolTable, buildInstructions, buildMachineCode, bigEndian) where

import Text.Megaparsec
import Data.Word (Word16)
import Data.Bits

import ISA.Instructions
import Parser.SymbolTable
import Parser.Lines
import Parser.Parser
import Parser.FirstPass
import Parser.SecondPass

parseFile :: FilePath -> IO [Line]
parseFile path = do
    contents <- readFile path
    case runParser programP path contents of
        Left err -> do
            -- errorBundlePretty turns the complex error object 
            -- into a human-readable string with line pointers.
            putStrLn $ errorBundlePretty err
            return []
        Right parsedLines -> do
            let lines = programAsLines parsedLines
            --mapM_ print lines
            return $ lines

buildSymbolTable :: [Line] -> SymbolTable
buildSymbolTable lines = Parser.FirstPass.firstPass lines

buildInstructions :: [Line] -> SymbolTable -> [Instruction]
buildInstructions lines symbolTable = Parser.SecondPass.secondPass lines symbolTable

-- each instruction is converted to a list of 16-bit words, which are then concatenated together
buildMachineCode :: [Instruction] -> [Word16]
buildMachineCode instructions = concatMap instructionToBits instructions

-- convert to big endian format (most significant byte first)
bigEndian :: [Word16] -> [Word16]
bigEndian = map (\w -> (((w .&. 0xFF00) `shiftR` 8) .|. ((w .&. 0xFF) `shiftL` 8)))