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
      Parser )
import Data.Semigroup ((<>))
import Cipher (cipher, invCipher)
import qualified Data.ByteString as BS (readFile, unpack)
import Util (fromHex, inputChunker, outputDeChunker, toHex)

data Options = Options {
    key        :: String,
    input      :: Input
}

data Mode = Encrypt Options | Decrypt Options

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
    help "Input raw hexadecimal data")

inputParser :: Parser Input
inputParser = fileInput <|> rawInput

optionParser :: Parser Options
optionParser = Options <$>
    strOption (
        long "key" <>
        short 'k' <>
        metavar "KEY" <>
        help "The AES key as a hexadecimal string"
    ) <*> inputParser


modeParser :: Parser Mode
modeParser = hsubparser (
    command "encrypt" (info (helper <*> (Encrypt <$> optionParser)) (progDesc "Encrypt data")) <>
    command "decrypt" (info (helper <*> (Decrypt <$> optionParser)) (progDesc "Decrypt data")))

getOpts :: Mode -> Options
getOpts (Encrypt a) = a
getOpts (Decrypt a) = a

main :: IO ()
main = do
    -- Parse args
    args <- execParser $ info (helper <*> modeParser) (progDesc "poggers")
    -- Either get the raw text or read the file
    states <- case input (getOpts args) of
            -- Convert raw string to State
            RawInput r -> pure $ inputChunker $ fromHex r
            -- Read the given file as a bytestring, unpack and convert to State
            FileInput f -> do
                content <- BS.readFile f
                pure $ inputChunker $ BS.unpack content
    -- Get key as [Byte]
    let k = fromHex $ key $ getOpts args
    -- Get result
    let result = case args of
            Encrypt opts -> map (`cipher` k) states
            Decrypt opts -> map (`invCipher` k) states
    putStrLn $ toHex $ outputDeChunker result