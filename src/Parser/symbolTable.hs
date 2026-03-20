module Parser.SymbolTable where

import qualified Data.Map as Map
import Data.Word (Word16)

-- Maps "loop" -> 0x0008 (address)
type SymbolTable = Map.Map String Word16