-------------------------------------------------------
--  Program Design & Data Structures (Course 1DL201)
--  Spring 2017 Home Assignment 3: Huffman Coding
--
--  Authors:
--    Ardalan Samimi Sadeh (DV1 C)
--    Gustav Lindqvist     (IT1 _)
--  Date:
--    February 3, 2017
-------------------------------------------------------
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
--  * Think of more edge cases / grading cases...
--  * Handle empty Tables in huffmanTree function

{- characterCounts s
   PURPOSE:   Counts the number of occurrences of each character in s
   PRE:       True
   POST:      A table that maps each character that occurs in s to the number of
              times the character occurs in s
   EXAMPLES:  characterCounts "Foobar" == T [('r',1),('a',1),('b',1),('o', 2),('F',1)]
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

{- REPRESENTATION CONVENTION: In the Huffman Tree Leaf c i, c represents the number of occurrences of the character represented by c. In Branch i r l, i is the combined number of character in the sub-trees r and l. Void is an empty sub-tree.
   REPRESENTATION INVARIANT:  Sub-trees with higher character counts do not occur at a lower level of the tree than sub-tress with lower character counts. The number of occurrences of each character must be above zero.
-}
data HuffmanTree = Void
                 | Leaf Int Char
                 | Branch Int HuffmanTree HuffmanTree deriving (Show)

{- huffmanTree t
   PURPOSE:   Creates a Huffman tree based on character counts in t.
   PRE:       t maps each key to a positive value.
   POST:      A Huffman tree based on the character counts in t.
   EXAMPLES:  huffmanTree (characterCounts "Foobar") == Branch 6 (Branch 2 (Leaf 1 'b') (Leaf 1 'F')) (Branch 4 (Leaf 2 'o') (Branch 2 (Leaf 1 'r') (Leaf 1 'a')))
              huffmanTree (characterCounts "H") == Leaf 1 'H'
 -}
huffmanTree :: Table Char Int -> HuffmanTree
huffmanTree table =
  let
    -- TODO: Empty table crashes the function. findLeast will handle that,
    -- but maybe unnecessary. If case better?
    queue = findLeast (Table.iterate table addToQueue PriorityQueue.empty)
  in
    -- Using uncurry here only because pattern matching
    -- below will be prettier, and less parenthesis-y.
    uncurry (buildHuffmanTree) queue where
      {- buildHuffmanTree t q
         PRE:       True.
         POST:      A Huffman tree based on the elements in q.
         EXAMPLES:  buildHuffmanTree (Leaf 1 'C',1) (PriorityQueue [(Leaf 1 'B',1),(Leaf 1 'A',1)]) == Branch 3 (Leaf 1 'A') (Branch 2 (Leaf 1 'C') (Leaf 1 'B'))
         VARIANT:   |q|
      -}
      buildHuffmanTree :: (HuffmanTree, Int) -> PriorityQueue HuffmanTree -> HuffmanTree
      buildHuffmanTree (t1, p1) q1
        | PriorityQueue.isEmpty q1 = t1
        | otherwise =
          let
            ((t2, p2), q2) = PriorityQueue.least q1
            t3             = mergeTree t1 t2
            q3             = PriorityQueue.insert q2 (t3, p2 + p1)
          in
            uncurry (buildHuffmanTree) (PriorityQueue.least q3)
      {- findLeast q
         PRE:       True.
         POST:      Element with least priority in q.
         EXAMPLES:  findLeast PriorityQueue.empty == ((Void, 0), PriorityQueue [])
      -}
      findLeast :: PriorityQueue HuffmanTree -> ((HuffmanTree, Int), PriorityQueue HuffmanTree)
      findLeast q | PriorityQueue.isEmpty q = ((Void, 0), q)
                  | otherwise               = PriorityQueue.least q
      {- addToQueue q x@(a, b)
         PRE:           b > 0.
         POST:          Inserts x in queue q.
         EXAMPLES:      addToQueue PriorityQueue.empty ('a', 2) == PriorityQueue [(Leaf 2 'a',2)]
      -}
      addToQueue :: PriorityQueue HuffmanTree -> (Char, Int) -> PriorityQueue HuffmanTree
      addToQueue q (c, i) = PriorityQueue.insert q ((Leaf i c), i)
      {- mergeTree t1 t2
         PRE:           True
         POST:          t1 and t2 merged.
         EXAMPLES:      mergeTree (Leaf 1 'a') (Leaf 2 'b') == Branch 3 (Leaf 1 'a') (Leaf 2 'b')
      -}
      mergeTree :: HuffmanTree -> HuffmanTree -> HuffmanTree
      mergeTree Void t = t
      mergeTree t Void = t
      mergeTree t1 t2  = Branch (priority t1 + priority t2) t1 t2
      {- priority t
         PRE:           t is not an empty subtree.
         POST:          Priority of t
         EXAMPLES:      priority (Leaf 1 'r') == 1
      -}
      priority :: HuffmanTree -> Int
      priority (Leaf p _)     = p
      priority (Branch p _ _) = p

{- codeTable h
   PURPOSE:   Creates a code table with each character in h mapped to its Huffman code.
   PRE:       True
   POST:      A table that maps each character in h to its Huffman code.
   EXAMPLES:  codeTable (huffmanTree (characterCounts "Hello World")) == T [('W', [False,False,False]), (' ', [False,False,True]), ('e', [False,True,False]), ('H', [False,True,True]), ('l', [True,False]), ('o', [True,True,False]), ('d', [True,True,True,False]), ('r', [True,True,True,True])]
 -}
codeTable :: HuffmanTree -> Table Char BitCode
codeTable h = mapCharacters h [] Table.empty
  where
  {- mapCharacters h b t
     PURPOSE:  Traverses the entire tree h and maps where each character is.
     PRE:      b and t are empty
     POST:     t with each character in h mapped to its Huffman code, accumulated in b
     EXAMPLES: mapCharacters (Branch 3 (Leaf 1 'A') (Branch 2 (Leaf 1 'C') (Leaf 1 'B'))) [] Table.empty == T [('A', [False]),('C', [True,False]),('B', [True,True])]
     VARIANT:  |h|
  -}
  mapCharacters :: HuffmanTree -> BitCode -> Table Char BitCode -> Table Char BitCode
  mapCharacters (Void)         _ _ = Table.empty
  mapCharacters (Leaf _ k)    [] t = Table.insert t k [False] -- Fix for single characters
  mapCharacters (Leaf _ k)     b t = Table.insert t k b
  mapCharacters (Branch _ l r) b t =
    mapCharacters r (addBit 1 b) (mapCharacters l (addBit 0 b) t)
      where
      {- addBit n l
         PRE:       n = {1, 0}
         POST:      l with False added to it if n == 0,
                    otherwise l with True added to it.
         EXAMPLES:  addBit 1 []     == [True]
                    addBit 0 [True] == [True, False]
      -}
      addBit :: Int -> [Bool] -> [Bool]
      addBit 0 b = b ++ [False]
      addBit 1 b = b ++ [True]

{- compress s
   PURPOSE:   Encodes the message in s.
   PRE:       True.
   POST:      A Huffman tree based on s, the Huffman coding of s under this tree.
   EXAMPLES:  compress "Hello World" == (Branch 11 (Branch 4 (Branch 2 (Leaf 1 'W') (Leaf 1 ' ')) (Branch 2 (Leaf 1 'e') (Leaf 1 'H'))) (Branch 7 (Leaf 3 'l') (Branch 4 (Leaf 2 'o') (Branch 2 (Leaf 1 'd') (Leaf 1 'r')))),[False,True,True,False,True,False,True,False,True,False,True,True,False,False,False,True,False,False,False,True,True,False,True,True,True,True,True,False,True,True,True,False])
 -}
compress :: String -> (HuffmanTree, BitCode)
compress [] = (Void, [])
compress s  =
  let
    tree = huffmanTree (characterCounts s)
    code = lookupCharacters s (codeTable tree)
      where
        {- lookupCharacters str t
           PRE:       True.
           POST:      Encoding of s based on mapped characters in t.
           EXAMPLES:  lookupCharacters "Hello World" (codeTable (huffmanTree (characterCounts "Hello World"))) == [False, True, True, False, True, False, True, False, True, False, True, True, False, False, False, True, False, False, False, True, True, False, True, True, True, True, True, False, True, True, True, False]
           VARIANT:   |s|
        -}
        lookupCharacters :: String -> Table Char BitCode -> BitCode
        lookupCharacters []      t = []
        lookupCharacters (c:str) t =
          let
            value = case Table.lookup t c of
                      Just x  -> x
                      Nothing -> []
          in
            value ++ (lookupCharacters str t)
  in
    (tree, code)

{- decompress h bits
   PURPOSE:   Decodes the message in bits from h.
   PRE:       bits is valid Huffman code for h.
   POST:      The decoding of bits under h.
   EXAMPLES:  uncurry decompress (compress "Hello World") == "Hello World"
 -}
decompress :: HuffmanTree -> BitCode -> String
decompress Void _              = "" -- fixes empty string issue
decompress _ []                = [] -- fixes issue with repeated chars, for example "xxx" => "xxxx"
decompress h@(Leaf _ k) (b:bs) = k : (decompress h bs)
decompress huffmanTree   bits  = traverseTree huffmanTree bits ""
  where
    {- traverseTree h b str
       PRE:           b is valid Huffman code for h.
       POST:          str consisting of characters from h mapped out in b.
       VARIANT:       |b|
    -}
    traverseTree :: HuffmanTree -> BitCode -> String -> String
    traverseTree (Leaf _ k)     []         str = str ++ [k]
    traverseTree (Leaf _ k)     bits       str = traverseTree huffmanTree bits (str ++ [k])
    traverseTree (Branch _ l r) (bit:bits) str | bit == False = traverseTree l bits str
                                               | otherwise    = traverseTree r bits str

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

testDecompress a = let (h, bits) = compress a in decompress h bits
