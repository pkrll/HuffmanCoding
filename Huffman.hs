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
--  * Fix compress/decompress - not inverses.
--  * Take care of edge cases such as when compressing/decompressing "" (empty string) (What to do when empty string??)
--

{- characterCounts s
   PURPOSE: counts the number of occurrences of each character in s
   PRE:  True
   POST: a table that maps each character that occurs in s to the number of
         times the character occurs in s
   EXAMPLES: characterCounts "Foobar" == T [('r',1),('a',1),('b',1),('o', 2),('F',1)]
 -}
characterCounts :: String -> Table Char Int

-- Which is better?
-- characterCounts' s = foldl countCharacter Table.empty s
--   where
--     countCharacter :: Table Char Int -> Char -> Table Char Int
--     countCharacter table char =
--       let
--         value = case Table.lookup table char of
--           Just x  -> x + 1
--           Nothing -> 1
--       in
--         Table.insert table char value

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
huffmanTree table =
  let
    -- Iterate over table and create a priority queue from the elements
    -- retrieve the least prioritized
    q = PriorityQueue.least (Table.iterate table populatePriorityQueue PriorityQueue.empty)
  in
    buildHuffmanTree q
      where
        {-
          buildHuffmanTree' ((t, i), q)
          PURPOSE:  Creates a Huffman tree from t and elements in q.
          PRE:      i > 0
          POST:     A Huffman Tree based on t and the elements in q.
          EXAMPLES: ...?
        -}
        buildHuffmanTree :: ((HuffmanTree, Int), PriorityQueue HuffmanTree) -> HuffmanTree
        buildHuffmanTree ((t1, p1), q1)
          | PriorityQueue.isEmpty q1 = t1 -- If the queue is empty, then only one tree remains
          | otherwise =
            let
              -- Get the least prioritized tree
              -- merge it with t1, insert it back in and call buildHuffmanTree again
              ((t2, p2), q2) = PriorityQueue.least q1
              t3             = ((mergeTree t1 t2), p1 + p2)
            in
              buildHuffmanTree (PriorityQueue.least (PriorityQueue.insert q2 t3))
        {- populatePriorityQueue q a@(c, p)
           PURPOSE:   populates a priority queue whose elements are Huffman trees based on a
           PRE:       p > 0
           POST:      The queue q with element (Leaf i p) inserted at priority p.
           EXAMPLES:  ...
        -}
        populatePriorityQueue :: PriorityQueue HuffmanTree -> (Char, Int) -> PriorityQueue HuffmanTree
        populatePriorityQueue q (c, i) =
          let
            leaf = Leaf i c
          in
            PriorityQueue.insert q (leaf, i)

{- mergeTree t1 t2
   PRE:           True
   POST:          t1 and t2 merged.
   EXAMPLES:      mergeTree (Leaf 1 'a') (Leaf 2 'b') == Branch 3 (Leaf 1 'a') (Leaf 2 'b')
-}
mergeTree :: HuffmanTree -> HuffmanTree -> HuffmanTree
mergeTree t1 t2 = Branch (priority t1 + priority t2) t1 t2

{- priority a
   PRE:           True
   POST:          The character count (priority) of a.
   EXAMPLES:      priority (Leaf (1, 'a')) == 1
-}
priority :: HuffmanTree -> Int
priority (Leaf a _)     = a
priority (Branch a _ _) = a

{- codeTable h
   PURPOSE:
   PRE:  True
   POST: a table that maps each character in h to its Huffman code
   EXAMPLES:
 -}
-- TODO: There is probably a better way to do this...
codeTable :: HuffmanTree -> Table Char BitCode
codeTable h = uncurriedTableInsertion Table.empty (mapCharacters h [])
  where
    {- uncurriedTableInsertion f t p@(k, v)
       PRE:           True?
       POST:          table t populated with elements in p
       EXAMPLES:      mapTable ==
       VARIANT:       |p|
    -}
    uncurriedTableInsertion :: Eq k => Table k v -> [(k, v)] -> Table k v
    -- The mapCharacters returns a list of tuples, uncurrying Table.insert
    -- allows for insertion of tuples (instead of having to use two arguments)
    uncurriedTableInsertion t [a]   = uncurry (Table.insert t) a
    uncurriedTableInsertion t (a:b) = uncurry (Table.insert (uncurriedTableInsertion t b)) a
    {- mapCharacters h b
       PRE:           None
       POST:          each character in h mapped to its Huffman code
       EXAMPLES:      mapCharacters ==
       VARIANT:       |
    -}
    -- Just traverses the whole tree and maps where each character is
    -- in the tree. A left turn is recorded as False (zero bit) and a
    -- right turn is True (one bit)
mapCharacters :: HuffmanTree -> BitCode -> [(Char, BitCode)]
mapCharacters (Leaf _ k) []    = [(k, [False])]
mapCharacters (Leaf _ k) b     = [(k, b)]
mapCharacters (Branch _ l r) b =
  mapCharacters l (addBit 0 b) ++ mapCharacters r (addBit 1 b)
    where
    {- addBit n l
       PRE:           n = {1, 0}
       POST:          l with False added to it, if n == 0,
                      otherwise l with True added to it.
       EXAMPLES:      addBit 1 []     == [True]
                      addBit 0 [True] == [True, False]
    -}
    addBit :: Int -> [Bool] -> [Bool]
    addBit 0 b = b ++ [False]
    addBit 1 b = b ++ [True]

{- compress s
   PURPOSE:
   PRE:  True
   POST: (a Huffman tree based on s, the Huffman coding of s under this tree)
   EXAMPLES:
 -}
compress :: String -> (HuffmanTree, BitCode)
compress s =
  let
    tree = huffmanTree (characterCounts s)
    code = lookupCharacters s (codeTable tree)
      where
        {- lookupCharacters s t
           PRE:           ???
           POST:          encoding of s based on mapped characters in t
           EXAMPLES:
           VARIANT:       |s|
        -}
        lookupCharacters :: String -> Table Char BitCode -> BitCode
        lookupCharacters []     table = []
        lookupCharacters (k:ks) table =
          let
            value = case Table.lookup table k of
                      Just x  -> x
                      Nothing -> []
          in
            value ++ (lookupCharacters ks table)
  in
    (tree, code)

{- decompress h bits
   PURPOSE:   decodes the message in bits from h
   PRE:       bits is a concatenation of valid Huffman code words for h
   POST:      the decoding of bits under h
   EXAMPLES:  uncurry decompress (compress "Hello World") == "Hello World"
 -}
decompress :: HuffmanTree -> BitCode -> String
decompress h@(Leaf _ k) []     = []
decompress h@(Leaf _ k) (b:bs) = k : (decompress h bs)
decompress h bits = traverseTree h bits ""
  where
    {- traverseTree h b str
       PRE:           b is a concatenation of valid Huffman code words for h
       POST:          str consisting of characters from h mapped out in b
       VARIANT:       |b|
-}
  traverseTree :: HuffmanTree -> BitCode -> String -> String
  traverseTree (Leaf _ k)     []         str = str ++ [k]
  traverseTree (Leaf _ k)     bits       str = traverseTree h bits (str ++ [k])
  traverseTree (Branch _ l r) (bit:bits) str | bit == False = traverseTree l bits str
                                             | otherwise    = traverseTree r bits str

testDecompress a = let (h, bits) = compress a in decompress h bits

singleLeaf :: HuffmanTree -> Bool
singleLeaf (Leaf _ _) = True
singleLeaf _          = False

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
