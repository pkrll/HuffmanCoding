-- DO NOT MODIFY THE FOLLOWING LINES

module Huffman(HuffmanTree, characterCounts, huffmanTree, codeTable, compress, decompress) where

import Table
import PriorityQueue

import Test.HUnit  -- if this causes an error, type 'cabal install HUnit' at the command line

{- REPRESENTATION CONVENTION:
     a bit code (of a character or string) is represented by a list of Booleans
   REPRESENTATION INVARIANT:
     the bit code is a concatenation of (0 or more) valid code words for some
     Huffman tree
 -}
type BitCode = [Bool]

-- END OF DO NOT MODIFY ZONE

--------------------------------------------------------------------------------

{- characterCounts s
   PURPOSE:
   PRE:  True
   POST: a table that maps each character that occurs in s to the number of
         times the character occurs in s
   EXAMPLES:
 -}
characterCounts :: String -> Table Char Int
characterCounts s = undefined


-- modify and add comments as needed
data HuffmanTree = HuffmanTree ()


{- huffmanTree t
   PURPOSE:
   PRE:  t maps each key to a positive value
   POST: a Huffman tree based on the character counts in t
   EXAMPLES:
 -}
huffmanTree :: Table Char Int -> HuffmanTree
huffmanTree t = undefined


{- codeTable h
   PURPOSE:
   PRE:  True
   POST: a table that maps each character in h to its Huffman code
   EXAMPLES:
 -}
codeTable :: HuffmanTree -> Table Char BitCode
codeTable h = undefined


{- compress s
   PURPOSE:
   PRE:  True
   POST: (a Huffman tree based on s, the Huffman coding of s under this tree)
   EXAMPLES:
 -}
compress :: String -> (HuffmanTree, BitCode)
compress s = undefined


{- decompress h bits
   PURPOSE:
   PRE:  bits is a concatenation of valid Huffman code words for h
   POST: the decoding of bits under h
   EXAMPLES:
 -}
decompress :: HuffmanTree -> BitCode -> String
decompress h bits = undefined


--------------------------------------------------------------------------------
-- Test Cases
-- You may add your own test cases here:
-- Follow the pattern and/or read about HUnit on the interwebs.
--------------------------------------------------------------------------------

-- characterCounts
test1 = TestCase $ assertEqual "characterCounts"
            (Just 7) (Table.lookup (characterCounts "this is an example of a huffman tree") ' ')

-- codeTable
-- while the precise code for ' ' may vary, its length (for the given example string) should always be 3 bits
test2 = TestCase $ assertEqual "codeTable"
            3 (maybe (-1) length (Table.lookup (codeTable (huffmanTree (characterCounts "this is an example of a huffman tree"))) ' '))

-- compress
-- while the precise code for the given example string may vary, its length should always be 135 bits
test3 = TestCase $ assertEqual "compress"
            135 (length (snd (compress "this is an example of a huffman tree")))

-- decompress
test4 =
    let s = "this is an example of a huffman tree"
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

test5 =
    let s = "xxx"
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

test6 =
    let s = ""
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

-- for running all the tests
runtests = runTestTT $ TestList [test1, test2, test3, test4, test5, test6]
