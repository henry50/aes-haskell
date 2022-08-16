module Cipher (cipher, inverseCipher, mkState, keyExpansion) where
import Data.Word (Word8)
import Data.Bits (Bits(xor, shiftR, (.&.), shiftL))
import Data.List (transpose)
import Numeric (showHex)
import Control.Arrow ((>>>))
import Prelude hiding (Word)

import Types (Key, KeySchedule, RoundKey, State, Word, Byte)

-- Chunk a list into a 2d list with items of size 4
chunk :: [a] -> [[a]]
chunk y =  map (take 4 . flip drop y . (*4)) [0..(length y `div` 4)-1]

-- Converts a list of bytes to a state
mkState :: [Byte] -> State
mkState = transpose . chunk

-- Convert a state to hex strings
stateHex :: State -> [[String]]
stateHex = (map.map) (`showHex` "")

-- Nk is the number of 32-bit words in the key
-- 32 bits = 4*8 bits, hence the div 4
nk :: Key -> Int
nk = (`div` 4) . length
-- Nb is the number of columns in the state, which is always 4
nb :: Int
nb = 4
-- Nr is the number of rounds, it is the key length + 6
nr :: Key -> Int
nr = (+6) . nk

-- The substitution box
sbox :: [Byte]
sbox = [0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76, 0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0, 0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15, 0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75, 0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84, 0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf, 0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8, 0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5, 0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2, 0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73, 0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb, 0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79, 0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08, 0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a, 0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e, 0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf, 0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16]

-- Key expansion generates Nb(Nr+1) RoundKeys
keyExpansion :: Key -> KeySchedule
keyExpansion k = roundify $ expand (nk k) $ chunk k where
    -- SubWord is SubBytes but on a 1d list
    subWord :: Word -> Word
    subWord = map ((!!) sbox . fromIntegral)
    -- rotWord cyclically rotates a word left by 1
    rotWord :: Word -> Word
    rotWord = take 4 . drop 1 . cycle
    -- Round constant array (index 0 is never used)
    rcon :: Int -> Byte
    rcon = (!!) [0x00, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36]
    -- Convert a list of words to a key schedule
    roundify :: [Word] -> KeySchedule
    roundify [] = []
    roundify x  = transpose (take 4 x) : roundify (drop 4 x)
    -- Expand the key into a list of words
    expand :: Int -> [Word] -> [Word]
    expand n ws
        | n == nb * (nr k + 1) = ws
        | otherwise          = expand (n+1) (ws ++ [expand' n ws])
    -- expand' gives the value of the ith word of the key schedule
    expand' :: Int -> [Word] -> Word
    expand' i xs = zipWith xor (xs !! (i-nk k)) (temp i xs) where
        temp :: Int -> [Word] -> Word
        temp i ws
            | i `mod` nk k == 0           = xorFirst (subWord (rotWord temp')) (rcon (i `div` nk k))
            | nk k > 6 && i `mod` nk k == 4 = subWord temp'
            | otherwise                 = temp'
            where
                -- temp' is the initial value of temp
                temp' = ws !! (i-1)
                -- XORs the given byte with the first byte of the given word.
                xorFirst :: Word -> Byte -> Word
                xorFirst []     _ = []
                xorFirst (x:xs) v = x `xor` v : xs

cipher :: State -> Key -> State
cipher state key = roundFunction 1 initState where
    initState = addRoundKey (head keySchedule) state
    keySchedule = keyExpansion key
    roundFunction :: Int -> State -> State
    roundFunction n =
        -- The final round does not have MixColumns
        if n == nr key then subBytes >>> shiftRows >>> addRoundKey (keySchedule !! n)
        else roundFunction (n+1) . (subBytes >>> shiftRows >>> mixColumns >>> addRoundKey (keySchedule !! n))

inverseCipher :: State -> Key -> State
inverseCipher = undefined

-- Subsitute each byte using the sbox
subBytes :: State -> State
subBytes = (map . map) ((!!) sbox . fromIntegral)

-- Cyclically shift each row, the first by 0, second by 1, third by 2, fourth by 3.
shiftRows :: State -> State
shiftRows st = zipWith (\row shift -> take nb $ drop shift $ cycle row) st [0..3]

mixColumns :: State -> State
mixColumns = transpose . map mix . transpose where
    -- Fixed set of Galois field multiplications
    operations :: [[Byte -> Byte]]
    operations = [[galoisMultiply 2, galoisMultiply 3, id, id],
                  [id, galoisMultiply 2, galoisMultiply 3, id],
                  [id, id, galoisMultiply 2, galoisMultiply 3],
                  [galoisMultiply 3, id, id, galoisMultiply 2]]
    -- Every row of operations is applied to the column.
    -- The results of the operations are XOR'd to give a new column.
    mix :: [Byte] -> [Byte]
    mix col = map (foldr xor 0 . flip (zipWith ($)) col) operations

-- Multiply two bytes in GF(256)
galoisMultiply :: Byte -> Byte -> Byte
galoisMultiply a b = result $ iterate (step1 >>> step2 >>> step3) (a, b, 0, 0) !! 8 where
    result (_, _, p, _) = p
    {-
        if((b & 1) == 1){
            p ^= a;
        }
    -}
    step1 (a, b, p, c) = (a, b, if (b .&. 1) == 1 then p `xor` a else p, c)
    {-
        b >>= 1;
        carry = a & 0x80;
        a <<= 1;
    -}
    step2 (a, b, p, c) = (shiftL a 1, shiftR b 1, p, a .&. 0x80)
    {-
        if(carry == 0x80){
            a ^= 0x1b;
        }
    -}
    step3 (a, b, p, c) = (if c == 0x80 then a `xor` 0x1b else a, b, p, c)

-- XORs the state with the round key
addRoundKey :: RoundKey -> State -> State
addRoundKey = zipWith (zipWith xor)
