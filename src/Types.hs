module Types where

import Data.Word ( Word8 )

-- Byte is just a synonym for Word8. Makes code more readable with the spec
type Byte = Word8

-- In the specification, a word refers to 4 bytes or 32 bits
type Word = [Byte]

-- The state is a 4x4 array of bytes, representing 128 bits of data
type State = [[Byte]]

-- The encryption key is stored as an array of bytes
type Key = [Byte]

-- The KeySchedule will be an (Nr+1) * 4 * 4 array of bytes
-- Each 4*4 sub-list is a Round Key, and there are Nr + 1 round keys
-- A round key is the same size as the state, which it is then XOR'd with
{-
For example:
    [
        [[a,b,c,d],[e,f,g,h],[i,j,k,l],[m,n,o,p]], -- Round key 1
        ...,
        [[a,b,c,d],[e,f,g,h],[i,j,k,l],[m,n,o,p]]  -- Round key Nr + 1
    ]
-}
type RoundKey = [[Byte]]
type KeySchedule = [RoundKey]