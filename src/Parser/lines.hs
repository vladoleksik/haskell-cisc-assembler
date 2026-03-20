module Parser.Lines where

import ISA.Instructions

data Line = Line {
    lineNumber :: Int,
    label :: Maybe String,
    instruction :: Maybe Instruction
} deriving (Show, Eq)