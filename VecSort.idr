import Data.Vect

insert : Ord elem =>
         (x : elem) -> 
         (xsSorted : Vect len elem) ->
         Vect (S len) elem
insert x [] = [x]
insert x (y :: xs) = case x < y of
                          False => y :: insert x xs
                          True => x :: y :: xs

insSort : Ord elem => Vect n elem -> Vect n elem
insSort [] = []
insSort (x :: xs) = let xsSorted = insSort xs in
                        insert x xsSorted


append : Vect n elem -> 
         Vect m elem ->
         Vect (n + m) elem
append [] ys = ys
append (x :: xs) ys = x :: append xs ys

-- Because the length is part of the type, you could also refer to it directly:

vecLength : Vect n elem -> Nat   -----<<<<<-------
vecLength {n} xs = n             -----<<<<<-------

-- The notation {n} in a pattern brings the implicit argument n into scope,
--  allowing you to use it directly.

-- More generally, you can give explicit values for implicit arguments by 
-- using the notation {n = value}, where n is the name of an implicit argument
