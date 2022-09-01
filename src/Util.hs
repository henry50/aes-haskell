module Util where
import Numeric (showHex, readHex)
import Data.List (transpose)
import Data.List.Split (chunksOf)
import Data.Char (chr)

import Types ( Byte, State ) 


-- Chunk a list into a 2d list with items of size 4
chunk :: [a] -> [[a]]
chunk y =  map (take 4 . flip drop y . (*4)) [0..(length y `div` 4)-1]

-- Convert a list of bytes to a list of states, padding with 0.
inputChunker :: [Byte] -> [State]
inputChunker x = map toState $ chunksOf 16 (x ++ getPadding x) where
    getPadding :: [Byte] -> [Byte]
    getPadding x = replicate n 0 where
        a = length x `mod` 16
        n = if a == 0 then 0 else 16 - a 

outputDeChunker :: [State] -> [Byte]
outputDeChunker = concatMap fromState

-- Convert hex string to state
hexToState :: String -> State
hexToState = toState . fromHex

-- Convert state to hex
stateToHex :: State -> String
stateToHex = toHex . fromState

-- Convert hex string WITHOUT SPACES to list of bytes
fromHex :: String -> [Byte]
fromHex (x:y:xs) = fst (head $ readHex [x, y]) : fromHex xs
fromHex _ =  []

-- Converts a list of bytes to a state
toState :: [Byte] -> State
toState = transpose . chunk

-- Convert a state to a list of bytes
fromState :: State -> [Byte]
fromState = concat . transpose

-- Convert list of bytes to hex string
toHex :: [Byte] -> String
toHex = concatMap (\x -> (if x < 0x10 then "0" else "") ++ showHex x "")

-- Convert list of bytes to actual string
toString :: [Byte] -> String
toString = map (chr . fromEnum)

-- Convert a state to hex strings
stateHex :: State -> [[String]]
stateHex = (map.map) (`showHex` "")