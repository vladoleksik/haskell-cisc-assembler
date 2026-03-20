module Parser.Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Char (isSpace)
import ISA.Registers
import ISA.Operands
import ISA.LabelOffsets
import ISA.InstrClasses.B1
import ISA.InstrClasses.B2
import ISA.InstrClasses.B3
import ISA.InstrClasses.B4
import ISA.Instructions
import Parser.Lines

-- Void means we aren't using custom error messages
-- String is the input type
type Parser = Parsec Void String

-- Space consumer: handles whitespace and comments starting with #
sc :: Parser ()
sc = L.space hspace1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Parse hexadecimal integer, explicitly consuming 0x prefix
hexP :: Parser Int
hexP = lexeme $ do
    _ <- string "0x"
    L.hexadecimal

-- try to parse a decimal integer, if that fails, try to parse a hexadecimal integer
integer :: Parser Int
integer = try hexP <|> (lexeme L.decimal)

-- Parse "R0" through "R15"
registerP :: Parser Register
registerP = lexeme $ do
    _ <- char 'R'
    n <- digitChar
    case registerFromInt (read [n]) of
        Just r -> return r
        Nothing -> fail "Invalid register"

-- parse a label (e.g. "loop:")
labelP :: Parser String
labelP = lexeme $ try $ do
    first <- letterChar
    rest <- many (alphaNumChar <|> char '_')
    _ <- char ':'
    return (first:rest)

--parse a direct register operand (e.g. "R3")
directRegP :: Parser Operand
directRegP = Direct <$> registerP

-- parse an indirect register operand (e.g. "(R5)")
indirectRegP :: Parser Operand
indirectRegP = do
    _ <- char '('
    r <- registerP
    _ <- char ')'
    return $ Indirect r

-- parse an indexed register operand (e.g. "15(R2)") with index to the left of the register
indexedLeftRegP :: Parser Operand
indexedLeftRegP = do
    idx <- integer
    _ <- char '('
    r <- registerP
    _ <- char ')'
    return $ Indexed r (fromIntegral idx)

-- parse an indexed register operand (e.g. "(R2)15") with index to the right of the register
indexedRightRegP :: Parser Operand
indexedRightRegP = do
    _ <- char '('
    r <- registerP
    _ <- char ')'
    idx <- integer
    return $ Indexed r (fromIntegral idx)

-- parse an indexed register operand that can have the index on either side of the register
indexedRegP :: Parser Operand
indexedRegP = try indexedLeftRegP <|> indexedRightRegP

-- parse an immediate operand (e.g. "42" or "0x2A")
immediateP :: Parser Operand
immediateP = Immediate <$> (fromIntegral <$> integer)

-- parse any operand
operandP :: Parser Operand
operandP = try indexedRegP <|> try directRegP <|> try indirectRegP <|> immediateP

--parse a label operand (e.g. "loop")
labelOperandP :: Parser LabelOffset
labelOperandP = do
    first <- letterChar
    rest <- many (alphaNumChar <|> char '_')
    return $ UnresolvedLabel (first:rest)

-- parse a type B1 instruction opcode (e.g. "MOV", "ADD", etc.) using the b1OpcodeFromString function
opcodeB1P :: Parser String
opcodeB1P = lexeme $ do
    code <- some (satisfy (not . isSpace)) -- parse until the first space
    case ISA.InstrClasses.B1.b1OpcodeFromString code of
        Just _ -> return code
        Nothing -> fail "Invalid opcode"

-- parse a type B2 instruction opcode (e.g. "JMP", "JEQ", etc.) using the b2OpcodeFromString function
opcodeB2P :: Parser String
opcodeB2P = lexeme $ do
    code <- some (satisfy (not . isSpace)) -- parse until the first space
    case ISA.InstrClasses.B2.b2OpcodeFromString code of
        Just _ -> return code
        Nothing -> fail "Invalid opcode"

-- parse a type B3 instruction opcode (e.g. "JMP", "JEQ", etc.) using the b3OpcodeFromString function
opcodeB3P :: Parser String
opcodeB3P = lexeme $ do
    code <- some (satisfy (not . isSpace)) -- parse until the first space
    case ISA.InstrClasses.B3.b3OpcodeFromString code of
        Just _ -> return code
        Nothing -> fail "Invalid opcode"

-- parse a type B4 instruction opcode (e.g. "JMP", "JEQ", etc.) using the b4OpcodeFromString function
opcodeB4P :: Parser String
opcodeB4P = lexeme $ do
    code <- some (satisfy (not . isSpace)) -- parse until the first space
    case ISA.InstrClasses.B4.b4OpcodeFromString code of
        Just _ -> return code
        Nothing -> fail "Invalid opcode"

-- parse a B1 instruction (e.g. "MOV R1, R2")
b1InstrP :: Parser Instruction
b1InstrP = do
    opcStr <- opcodeB1P
    let Just opc = ISA.InstrClasses.B1.b1OpcodeFromString opcStr
    opd <- operandP
    case ISA.InstrClasses.B1.mutates opc && (not . isWritable) opd of
        True -> fail "Invalid destination operand for this instruction"
        False -> do
            _ <- char ',' <* sc
            ops <- operandP
            return $ B1Instr $ B1Instruction opc opd ops

-- parse a B2 instruction (e.g. "JMP loop")
b2InstrP :: Parser Instruction
b2InstrP = do
    opcStr <- opcodeB2P
    let Just opc = ISA.InstrClasses.B2.b2OpcodeFromString opcStr
    opd <- operandP
    case ISA.InstrClasses.B2.mutates opc && (not . isWritable) opd of
        True -> fail "Invalid destination operand for this instruction"
        False -> return $ B2Instr $ B2Instruction opc opd

-- parse a B3 instruction (e.g. "JMP loop")
b3InstrP :: Parser Instruction
b3InstrP = do
    opcStr <- opcodeB3P
    let Just opc = ISA.InstrClasses.B3.b3OpcodeFromString opcStr
    targetLabel <- labelOperandP
    return $ B3Instr $ B3Instruction opc targetLabel

-- parse a B4 instruction (e.g. "JMP loop")
b4InstrP :: Parser Instruction
b4InstrP = do
    opcStr <- opcodeB4P
    let Just opc = ISA.InstrClasses.B4.b4OpcodeFromString opcStr
    return $ B4Instr $ B4Instruction opc

-- parse any instruction
instructionP :: Parser Instruction
instructionP = try b1InstrP <|> try b2InstrP <|> try b3InstrP <|> b4InstrP

-- parse a line which can have an optional label followed by an instruction, or just a label, or just an instruction, or be empty
lineP :: Parser (Maybe String, Maybe Instruction)
lineP = do
    sc
    lbl <- optional labelP
    sc
    instr <- optional instructionP
    sc
    return (lbl, instr)

-- parse multiple lines
programP :: Parser [(Maybe String, Maybe Instruction)]
programP = lineP `sepEndBy` eol <* eof

-- number the lines and convert to Line data type
programAsLines :: [( Maybe String, Maybe Instruction )] -> [Line]
programAsLines parsedLines = zipWith toLine [1..] parsedLines
  where
    toLine n (lbl, instr) = Line n lbl instr