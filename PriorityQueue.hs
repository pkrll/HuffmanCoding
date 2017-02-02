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

-- the type of priority queues with elements of type a (and priorities
-- of type Int)

{- REPRESENTATION CONVENTION: ... description of how the datatype represents data ...
   REPRESENTATION INVARIANT:  None
-}
data PriorityQueue a = PriorityQueue [(a, Int)] deriving (Show)

empty = PriorityQueue []

isEmpty (PriorityQueue q) = null q

-- It will return an ordered PriorityQueue
-- VARIANT: |a|?
insert (PriorityQueue q) p = PriorityQueue (ins q p)
  where
    {- ins a b@(c, d)
       PRE:       True
       POST:      a with element b inserted, in ascending order by d.
       EXAMPLES:  ins [] ('A', 2) == [('a', 2)] (Do we need example?)
       VARIANT:   |a|?
    -}
    ins :: [(a, Int)] -> (a, Int) -> [(a, Int)]
    ins []    p = [p]
    ins (h:q) p =
      if snd h > snd p
        then p:h:q
        else h:(ins q p)

least (PriorityQueue (h:q)) = (h, PriorityQueue q)
