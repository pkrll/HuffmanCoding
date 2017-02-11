-------------------------------------------------------
--  Program Design & Data Structures (Course 1DL201)
--  Spring 2017 Home Assignment 3: Huffman Coding
--
--  Authors:
--    Ardalan Samimi Sadeh (DV1 C)
--    Gustav Lindqvist     (IT1 B)
--  Date:
--    February 3, 2017
-------------------------------------------------------
-- DO NOT MODIFY THE FOLLOWING LINES

module PriorityQueue(PriorityQueue, empty, isEmpty, insert, least) where

--------------------------------------------------------------------------------
-- interface
--------------------------------------------------------------------------------

-- the empty priority queue
empty :: PriorityQueue a

{- isEmpty q
   PURPOSE: determines whether q is empty
   PRE:  True
   POST: True if and only if q is empty
   EXAMPLES: isEmpty empty == True
             isEmpty (insert empty ('a',1)) == False
 -}
isEmpty :: PriorityQueue a -> Bool

{- insert q (x,p)
   PURPOSE: inserts x with priority p into q
   PRE:  True
   POST: the queue q with element x inserted at priority p
   EXAMPLES: insert empty ('a',1) is the queue that contains just 'a' with
             priority 1
 -}
insert :: PriorityQueue a -> (a, Int) -> PriorityQueue a

{- least q
   PURPOSE: extracts an element of minimum priority from q
   PRE:  q is not empty
   POST: ((x,p), q’), where x is an element of minimum priority in q, p is the
         priority of x, and q’ is q without x
   EXAMPLES: least (insert empty ('a',1)) == (('a',1), empty)
 -}
least :: PriorityQueue a -> ((a, Int), PriorityQueue a)

-- END OF DO NOT MODIFY ZONE

--------------------------------------------------------------------------------
-- implementation
--------------------------------------------------------------------------------

{- REPRESENTATION CONVENTION:
     A priority queue consists of a list of pairs where the second element is the priority of the element.
   REPRESENTATION INVARIANT:
     An element with higher priority is always behind one with a lower priority in the list.
-}
data PriorityQueue a = PriorityQueue [(a, Int)] deriving (Show)

empty = PriorityQueue []

isEmpty (PriorityQueue q) = null q

-- It will return an ordered PriorityQueue
insert (PriorityQueue q) p = PriorityQueue (ins q p)
  where
    {- ins a b@(c, d)
       PRE:       True
       POST:      a with element b inserted, in ascending order by d.
       EXAMPLES:  ins [] ('A', 2) == [('a', 2)]
       VARIANT:   |a|
    -}
    ins :: [(a, Int)] -> (a, Int) -> [(a, Int)]
    ins []    p = [p]
    ins (h:q) p =
      if snd p <= snd h
        then p:h:q
        else h:(ins q p)

least (PriorityQueue (h:q)) = (h, PriorityQueue q)
