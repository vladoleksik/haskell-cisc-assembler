module FileWriter where

import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put (runPut, putWord16be, putWord16le)
import Data.Word (Word16)

-- Change 'putWord16be' to 'putWord16le' if you want Little-Endian
wordsToByteString :: [Word16] -> BL.ByteString
wordsToByteString ws = runPut $ mapM_ putWord16be ws

writeBinaryFile :: FilePath -> [Word16] -> IO ()
writeBinaryFile path ws = BL.writeFile path (wordsToByteString ws)