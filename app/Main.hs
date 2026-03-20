module Main where

import Options.Applicative
import qualified Assembler
import qualified FileWriter
import qualified Data.Map as Map
import Numeric (showHex)

-- 1. The Data Structure for our CLI
data Options = Options
  { inputFile   :: FilePath
  , outputFile  :: FilePath
  , isLittle    :: Bool
  , showSymbols :: Bool
  , showIR      :: Bool
  }

-- 2. The Parser logic
optionsParser :: Parser Options
optionsParser = Options
  <$> strOption (short 'i' <> metavar "INPUT" <> help "Input assembly file")
  <*> strOption (short 'o' <> metavar "OUTPUT" <> help "Output binary file")
  <*> switch    (long "le" <> help "Use Little-Endian (default is Big-Endian)")
  <*> switch    (short 's' <> help "Print symbol table")
  <*> switch    (short 'a' <> help "Print intermediate representation")

-- Help and Version Info
optsInfo :: ParserInfo Options
optsInfo = info (optionsParser <**> helper <**> versionP)
  ( fullDesc 
  <> progDesc "A simple custom ISA assembler"
  <> header "Assembler v0.1.0 - Author: Vlad-Andrei Oleksik" )
  where
    -- This creates a special parser that prints and exits
    versionP = infoOption "Assembler v0.1.0 | Author: Vlad-Andrei Oleksik" 
               (short 'v' <> long "version" <> help "Show version")

main :: IO ()
main = execParser optsInfo >>= runWithOpts

runWithOpts :: Options -> IO ()
runWithOpts opts = do
      -- 1. Parse
      lines <- Assembler.parseFile (inputFile opts)
      if showIR opts then mapM_ print lines else return ()

      -- 2. Symbol Table
      let symbolTable = Assembler.buildSymbolTable lines
      if showSymbols opts 
        then do
          putStrLn "Symbol Table:"
          mapM_ (\(lbl, addr) -> putStrLn $ lbl ++ ": " ++ show addr) (Map.toList symbolTable)
        else return ()

      -- 3. Assemble
      let instructions = Assembler.buildInstructions lines symbolTable
      
      -- 4. Handle Endianness
      let rawCode = Assembler.buildMachineCode instructions
      let machineCode = if isLittle opts 
                        then rawCode
                        else Assembler.bigEndian rawCode

      if length machineCode == 0
        then putStrLn "Errors found. No machine code generated."
        else do
          FileWriter.writeBinaryFile (outputFile opts) machineCode
          putStrLn $ "Assembled to " ++ outputFile opts

formatHex :: (Integral a, Show a) => a -> String
formatHex x = let h = showHex x "" in replicate (4 - length h) '0' ++ h