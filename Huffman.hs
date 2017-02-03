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
characterCounts [] = Table.empty
characterCounts (s':s) = let
                           t = characterCounts s
                           v = case Table.lookup t s' of
                               Just x -> x+1
                               Nothing -> 1
                         in
                           Table.insert t s' v


-- modify and add comments as needed
data HuffmanTree = Void
                 | Leaf Int Char
                 | Branch Int HuffmanTree HuffmanTree deriving Show


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
codeTable Void         = Table.empty
codeTable (Leaf c i)   = undefined
codetable (Branch l i r) = undefined


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
decompress Void _ = []
decompress _   [] = []
decompress h@(Leaf i c) (b':b) = c:(decompress h b) --Ursprungs h
decompress h b = fun h b ""
	where
		fun :: HuffmanTree -> BitCode -> String -> String
		fun (Leaf i c) [] s = s ++ [c]
		fun (Leaf i c) b s = fun h b (s ++ [c]) --Hela trädet
		fun (Branch i l r) (b':b) s | b' == True = fun r b s
					  | otherwise = fun l b s
                       
a = (Branch 12 (Branch 5 (Branch 2 (Leaf 1 ' ') (Leaf 1 'e')) (Leaf 3 'l')) (Branch 7 (Branch 3 (Leaf 1 'H') (Leaf 2 'o')) (Branch 4 (Branch 2 (Leaf 1 '!') (Leaf 1 'd')) (Branch 2 (Leaf 1 'r') (Leaf 1 'W')))))
b = [True,False,False,False,False,True,False,True,False,True,True,False,True,False,False,False,True,True,True,True,True,False,True,True,True,True,False,False,True,True,True,False,True,True,True,False,False]

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
