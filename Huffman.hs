-------------------------------------------------------
--  Program Design & Data Structures (Course 1DL201)
--  Spring 2017 Home Assignment 3: Huffman Coding
--
--  Authors:
--    Ardalan Samimi Sadeh (DV1 C)
--    Gustav Lindqvist     (IT1 B)
-------------------------------------------------------
-- DO NOT MODIFY THE FOLLOWING LINES

module Huffman(HuffmanTree, characterCounts, huffmanTree, codeTable, compress, decompress) where

import Table
import PriorityQueue
import Data.Maybe  -- Added by student
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
   PURPOSE:   Counts the number of occurrences of each character in s
   PRE:       True
   POST:      A table that maps each character that occurs in s to the number of
              times the character occurs in s
   EXAMPLES:  characterCounts "Foobar" == A table that maps characters 'F' to 1, 'o' to 2, 'b' to 1, 'a' to 1 and 'r' to 1 (and that contains no other keys).
              characterCounts "" == Table.empty
 -}
characterCounts :: String -> Table Char Int
characterCounts []     = Table.empty
characterCounts (k:ks) =
  let
    table = characterCounts ks
    value = fromMaybe 0 (Table.lookup table k) + 1
  in
    Table.insert table k value

{- REPRESENTATION CONVENTION:
     In the Huffman Tree Leaf i c, i represents the number of occurrences of the character represented by c. In Branch i r l, i is the combined number of character in the sub-trees r and l. Void is an empty sub-tree.
   REPRESENTATION INVARIANT:
     Sub-trees with higher character counts do not occur at a lower level of the tree than sub-tress with lower character counts. The number of occurrences of a character must be above zero.
-}
data HuffmanTree = Void
                 | Leaf Int Char
                 | Branch Int HuffmanTree HuffmanTree deriving (Show)

{- huffmanTree t
   PURPOSE:   Creates a Huffman tree based on character counts in t.
   PRE:       t maps each key to a positive value.
   POST:      A Huffman tree based on the character counts in t.
   EXAMPLES:  huffmanTree (characterCounts "Foobar") == Branch 6 (Leaf 2 'o') (Branch 4 (Branch 2 (Leaf 1 'a') (Leaf 1 'r')) (Branch 2 (Leaf 1 'F') (Leaf 1 'b')))
              huffmanTree (characterCounts "HHH") == Leaf 3 'H'
 -}
huffmanTree :: Table Char Int -> HuffmanTree
huffmanTree table =
  let
    queue = findLeast (Table.iterate table addToQueue PriorityQueue.empty)
  in
    -- Using uncurry here only because pattern matching
    -- below will be prettier, and less parenthesis-y.
    uncurry (buildHuffmanTree) queue where
      {- buildHuffmanTree t q
         PRE:       True.
         POST:      A single Huffman tree based on the elements in q.
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
         PRE:       b > 0.
         POST:      Inserts x in queue q.
         EXAMPLES:  addToQueue PriorityQueue.empty ('a', 2) == PriorityQueue [(Leaf 2 'a', 2)]
      -}
      addToQueue :: PriorityQueue HuffmanTree -> (Char, Int) -> PriorityQueue HuffmanTree
      addToQueue q (c, i) = PriorityQueue.insert q ((Leaf i c), i)
      {- mergeTree t1 t2
         PRE:       True
         POST:      t1 and t2 merged.
         EXAMPLES:  mergeTree (Leaf 1 'a') (Leaf 2 'b') == Branch 3 (Leaf 1 'a') (Leaf 2 'b')
      -}
      mergeTree :: HuffmanTree -> HuffmanTree -> HuffmanTree
      mergeTree Void t = t
      mergeTree t Void = t
      mergeTree t1 t2  = Branch (priority t1 + priority t2) t1 t2
      {- priority t
         PRE:       t is not an empty subtree.
         POST:      Priority of t
         EXAMPLES:  priority (Leaf 1 'r') == 1
      -}
      priority :: HuffmanTree -> Int
      priority (Leaf p _)     = p
      priority (Branch p _ _) = p

{- codeTable h
   PURPOSE:   Creates a code table with each character in h mapped to its Huffman code.
   PRE:       True
   POST:      A table that maps each character in h to its Huffman code.
   EXAMPLES:  codeTable (huffmanTree (characterCounts "Foo")) == A table that maps character 'F' to [False], 'o' to [True] (and that contains no other keys).
 -}
codeTable :: HuffmanTree -> Table Char BitCode
codeTable h = mapCharacters h [] Table.empty
  where
  {- mapCharacters h b t
     PURPOSE:  Traverses the entire tree h and maps where each character is.
     PRE:      b and t are empty
     POST:     t with each character in h mapped to its Huffman code, accumulated in b
     EXAMPLES: mapCharacters (Branch 3 (Leaf 1 'A') (Branch 2 (Leaf 1 'C') (Leaf 1 'B'))) [] Table.empty == A table that maps 'A' to [False], 'C' to [True, False], 'B' to [True, True] (and that contains no other keys).
     VARIANT:  |h|
  -}
  mapCharacters :: HuffmanTree -> BitCode -> Table Char BitCode -> Table Char BitCode
  mapCharacters (Void)         _ _ = Table.empty
  mapCharacters (Leaf _ k)    [] t = Table.insert t k [False] -- Fix for single characters
  mapCharacters (Leaf _ k)     b t = Table.insert t k b
  mapCharacters (Branch _ l r) b t = mapCharacters r (b ++ [True]) (mapCharacters l (b ++ [False]) t)

{- compress s
   PURPOSE:   Encodes the message in s.
   PRE:       True.
   POST:      A Huffman tree based on s, the Huffman coding of s under this tree.
   EXAMPLES:  compress "foobar" == (Branch 6 (Leaf 2 'o') (Branch 4 (Branch 2 (Leaf 1 'a') (Leaf 1 'r')) (Branch 2 (Leaf 1 'f') (Leaf 1 'b'))), [True, True, False, False, False, True, True, True, True, False, False, True, False, True])
              compress ""       == (Void,[])
              compress "xxx"    == (Leaf 3 'x', [False, False, False])
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
           EXAMPLES:  lookupCharacters "Foobar" (codeTable (huffmanTree (characterCounts "Foobar"))) == [True, True, False, False, False, True, True, True, True, False, False, True, False, True]
           VARIANT:   |s|
        -}
        lookupCharacters :: String -> Table Char BitCode -> BitCode
        lookupCharacters []      t = []
        lookupCharacters (c:str) t = (fromMaybe [] (Table.lookup t c)) ++ (lookupCharacters str t)
  in
    (tree, code)

{- decompress h bits
   PURPOSE:   Decodes the message in bits from h.
   PRE:       bits is valid Huffman code for h.
   POST:      The decoding of bits under h.
   EXAMPLES:  uncurry decompress (compress "Foobar") == "Foobar"
 -}
decompress :: HuffmanTree -> BitCode -> String
decompress Void _              = "" -- fixes issue with empty strings
decompress _ []                = [] -- fixes an issue with repeated chars
decompress h@(Leaf _ k) (b:bs) = k : (decompress h bs)
decompress huffmanTree   bits  = traverseTree huffmanTree bits ""
  where
    {- traverseTree h b str
      PRE:      b is valid Huffman code for h.
      POST:     str consisting of characters from h mapped out in b.
      EXAMPLES: traverseTree (Branch 6 (Leaf 2 'o') (Branch 4 (Branch 2 (Leaf 1 'a') (Leaf 1 'r')) (Branch 2 (Leaf 1 'f') (Leaf 1 'b'))), [True, True, False, False, False, True, True, True, True, False, False, True, False, True) == "Foobar"
      VARIANT:  |b|
    -}
    traverseTree :: HuffmanTree -> BitCode -> String -> String
    traverseTree (Leaf _ k)     []         str = reverse (k:str)
    traverseTree (Leaf _ k)     bits       str = traverseTree huffmanTree bits (k:str)
    traverseTree (Branch _ l r) (bit:bits) str | bit       = traverseTree r bits str
                                               | otherwise = traverseTree l bits str

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

test7 =
    let s = "Ölands Ålar äter älgkött med skålar."
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

-- for running all the tests
runtests = runTestTT $ TestList [test1, test2, test3, test4, test5, test6, test7]
