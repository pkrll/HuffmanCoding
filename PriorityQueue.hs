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
-- modify and add comments as needed
type PriorityQueue a = ()

empty = undefined

isEmpty q = undefined

insert q (x,p) = undefined

least q = undefined
