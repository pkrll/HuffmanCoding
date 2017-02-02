-- DO NOT MODIFY THE FOLLOWING LINES

module Huffman(HuffmanTree, characterCounts, huffmanTree, codeTable, compress, decompress) where

import Table
import PriorityQueue

import Debug.Trace
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

-- TODO:
--  * Decompress/Compress must be inverse functions
--  * Take care of edge cases such as when compressing/decompressing "" (empty string) (What to do when empty string??)
--  * Does the tree need to be balanced?
--

{- characterCounts s
   PURPOSE: counts the number of occurrences of each character in s
   PRE:  True
   POST: a table that maps each character that occurs in s to the number of
         times the character occurs in s
   EXAMPLES: characterCounts "Foobar" == T [('r',1),('a',1),('b',1),('o', 2),('F',1)]
 -}
characterCounts :: String -> Table Char Int

characterCounts []     = Table.empty
characterCounts (k:ks) =
  let
    table = characterCounts ks
    value = case Table.lookup table k of
              Just x  -> x + 1
              Nothing -> 1
  in
    Table.insert table k value

{- REPRESENTATION CONVENTION: In the Huffman Tree Leaf c i, c represents the number of occurrences of the character represented by c. In Branch i r l, i is the combined number of character in the sub-trees r and l.
   REPRESENTATION INVARIANT:  Sub-trees with higher character counts do not occur at a lower level of the tree than sub-tress with lower character counts.
-}
data HuffmanTree = Leaf Int Char
                 | Branch Int HuffmanTree HuffmanTree deriving (Show)

{- huffmanTree t
   PURPOSE:   Creates a Huffman tree based on character counts in t
   PRE:       t maps each key to a positive value
   POST:      A Huffman tree based on the character counts in t
   EXAMPLES:  huffmanTree (characterCounts "Foobar") == Branch 6 (Branch 2 (Leaf 1 'b') (Leaf 1 'F')) (Branch 4 (Leaf 2 'o') (Branch 2 (Leaf 1 'r') (Leaf 1 'a')))
              huffmanTree (characterCounts "H") == Leaf 1 'H'
 -}
huffmanTree :: Table Char Int -> HuffmanTree
huffmanTree table = undefined

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
   PURPOSE:   decodes the message in bits from h
   PRE:       bits is a concatenation of valid Huffman code words for h
   POST:      the decoding of bits under h
   EXAMPLES:  uncurry decompress (compress "Hello World") == "Hello World"
 -}
decompress :: HuffmanTree -> BitCode -> String
decompress h b = undefined

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
