module Main where

import Options.Applicative
    ( helper,
      execParser,
      progDesc,
      info,
      command,
      hsubparser,
      help,
      metavar,
      short,
      long,
      strOption,
      Alternative((<|>)),
      Parser, switch )
import Data.Semigroup ((<>))
import Cipher (cipher, invCipher)
import qualified Data.ByteString as BS (readFile, unpack)
import Util (fromHex, inputChunker, outputDeChunker, toHex)

data Options = Options {
    key        :: String,
    input      :: Input,
    decrypt    :: Bool
}

-- Input to the program can either be a file or raw input
data Input = FileInput FilePath | RawInput String deriving Show

fileInput :: Parser Input
fileInput = FileInput <$> strOption (
    long "file" <>
    short 'f' <>
    metavar "FILENAME" <>
    help "Input file path")

rawInput :: Parser Input
rawInput = RawInput <$> strOption (
    long "raw" <>
    short 'r' <>
    metavar "DATA" <>
    help "Input as hexadecimal string")

inputParser :: Parser Input
inputParser = fileInput <|> rawInput

optionParser :: Parser Options
optionParser = Options <$>
    strOption (
        long "key" <>
        short 'k' <>
        metavar "KEY" <>
        help "Key as a hexadecimal string"
    ) <*>
    inputParser <*>
    switch (
        long "decrypt" <>
        short 'd' <>
        help "Decrypt"
    )

main :: IO ()
main = do
    -- Parse args
    args <- execParser $ info (helper <*> optionParser) (progDesc "128, 192 and 256-bit CBC AES implementation.")
    -- Either get the raw text or read the file
    states <- case input args of
            -- Convert raw string to State
            RawInput r -> pure $ inputChunker $ fromHex r
            -- Read the given file as a bytestring, unpack and convert to State
            FileInput f -> do
                content <- BS.readFile f
                pure $ inputChunker $ BS.unpack content
    -- Get key as [Byte]
    let k = fromHex $ key args
    -- Get result
    let result = if decrypt args
        then map (`invCipher` k) states
        else map (`cipher` k) states
    putStrLn $ toHex $ outputDeChunker result