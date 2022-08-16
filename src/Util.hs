module Util where
import Numeric (showHex, readHex)
import Data.List ( transpose )

import Types ( Byte, State ) 

-- Chunk a list into a 2d list with items of size 4
chunk :: [a] -> [[a]]
chunk y =  map (take 4 . flip drop y . (*4)) [0..(length y `div` 4)-1]

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

-- Convert a state to hex strings
stateHex :: State -> [[String]]
stateHex = (map.map) (`showHex` "")