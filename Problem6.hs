{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Problem6 where
import Data.List.NonEmpty (nonEmpty)

{-------------------------------------------------------------------------------

CS:3820 Fall 2021 Problem of the Week, Week 6
=============================================

This week's problem concerns the pony express---or at least, horse-powered mail
delivery.  For our problem, we're going to consider a rider following a set
route with many stations where they can change horses.  Horses can only run a
certain distance, so riders have to change horses occasionally.  However,
changing horses takes time, so the rider wants to minimize the number of
changes.    Our goal is to figure out how the rider can minimize the number of
horse changes without exceeding the maximum distance a horse can run.

We'll follow basically the same approach we did for solving Sudoku puzzles:
enumerate all the possible routes, then restrict to those routes that don't
overwork the horses.

-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------

Problem 6-1
-----------

The first task is to write a function `routes` that enumerates all possible
routes with a given set of stops.  In a function call

    routes start end stops

the `start` and `end` arguments give the starting and ending points of the
route, and the list `stops` contains every possible stop along the route.
Because our route is fixed, each point is identified by a single integer
coordinate.

The only restriction is that every route you return *must* begin with the
starting point and end with the ending point.  You should return every possible
(in-order!) combination of other stops.  For example:

>>> routes 0 10 [2,5,8]
[[0,2,5,8,10],[0,2,5,10],[0,2,8,10],[0,2,10],[0,5,8,10],[0,5,10],[0,8,10],[0,10]]

Of course, you don't have to return the routes in that order, so long as they're
all there.

You may assume that neither the start or end is in the list of stops, and that
the stops are all between the start and end.

-------------------------------------------------------------------------------}
routesHelper :: [Int] -> [[Int]] --Helper function to turn int list to array
routesHelper [] = [[]] --Base case for empty
routesHelper (nonEmptyList:list) = map (nonEmptyList:) (routesHelper list) ++ (routesHelper list) --Case for all other combinations

routes :: Int -> Int -> [Int] -> [[Int]]
routes start end [] = [[start, end]] --Accomodates for the base case // defines start to end of the route
routes start end ss = map((start:) . (++[end])) nonEmptyList --Creates a map by appending the lists
                        where nonEmptyList = routesHelper ss --Uses helper function to filter for non empty lists

-- >>> routes 0 9 [1,4,7]
-- [[0,1,4,7,9],[0,1,4,9],[0,1,7,9],[0,1,9],[0,4,7,9],[0,4,9],[0,7,9],[0,9]]

-- >>> routes 1 20 [5,10,15]
-- [[1,5,10,15,20],[1,5,10,20],[1,5,15,20],[1,5,20],[1,10,15,20],[1,10,20],[1,15,20],[1,20]]

{-------------------------------------------------------------------------------

Problem 6-2
-----------

Many of the routes returned by the `routes` function will include legs that
exceed the distance a horse can run.  The next task is to write a function that
excludes these routes.  You'll do so in two steps.

First, you'll write a function `pairwise` that returns each pair of adjacent 
elements in a list.  Intuitively, each pair corresponds to a single leg in a
route.  For example:

>>> pairwise [0,1,2,3,4] 
[(0,1),(1,2),(2,3),(3,4)]

Note that each element (other than the first and last) appears twice: once with
its preceding element and once with its following element.  You may assume that
the input list is of length at least 2 (the start and end points).  You may find
nested pattern matching useful in writing `pairwise`.

Second, you'll write a function `allOkay d ps` which returns `True` if each pair
`(x,y)` in `ps` has a difference `y - x` less than or equal to `d`.  For
examples:

>>> allOkay 3 (pairwise [0,2,5,8,10]) 
True

>>> allOkay 3 (pairwise [0,5,8,10]) 
False

-------------------------------------------------------------------------------}

pairwise :: [a] -> [(a, a)]
pairwise [] = [] --Base case for given and returning an empty list
pairwise [a] = [] --Second base case for returning empty list with given input
pairwise (a:b:ss) = (a,b) : pairwise(b:ss) --Recursively iterates on the upcoming value and evaluates to the end of the list

-- >>> pairwise [0,1,2,3,4,5,6,7,8,9] 
-- [(0,1),(1,2),(2,3),(3,4),(4,5),(5,6),(6,7),(7,8),(8,9)]

allOkay :: Int -> [(Int, Int)] -> Bool
allOkay d [] = True --Base case defaults to true option
allOkay d ((a,b):ss) = ((b-a) <= d) && allOkay d ss --Compares the values in the list and returns a respective boolean

-- >>> allOkay 3 (pairwise [0,4,7,9]) 
-- False

{-------------------------------------------------------------------------------

Problem 6-3
-----------

Now we have a collection of possible routes, but many of them will involve more
horse changes than necessary.  To pick *a* shortest route, we need to pick a
route with the minimum number of stops.  Your next task is to define a function

    minimumBy f xs

that returns an element `x` of `xs` that minimizes `f x`.  That is to say:  if
`minimum f xs` returns `x`, then for every other `y` in `xs`, `f x <= f y`.

You may assume that `xs` contains at least one element.

-------------------------------------------------------------------------------}

minimumBy :: Ord a => (b -> a) -> [b] -> b --Returns a minimized element of xs
minimumBy f [baseValue] = baseValue --Creates a base value for first base case
minimumBy f (x:y:z) | f x > f y = minimumBy f(y:z) --Case for greater than minimum
                    | f x <= f y = minimumBy f(x:z) --Case for less than or equal to the minimum stops

{-------------------------------------------------------------------------------

Problem 6-4
-----------

Finally, we can put the pieces together: the function

    fewestStops start end distance stops

should return *a* route from `start` to `end`, with the minimum number of stops,
drawn from `stops`, in which no leg exceeds length `d`.  Your route *must*
include the start and end points.  For example:

>>> fewestStops 0 10 5 [2,5,8]
[0,5,10]

>>> fewestStops 0 100 25 [5,23,30,36,46,55,62,64,69,70,75]
[0,5,30,55,75,100]

For this example, you do not need to return this exact route.  However, you
should return a route of length 6.

-------------------------------------------------------------------------------}

fewestStops :: Int -> Int -> Int -> [Int] -> [Int] --Returns the start to end route with minimum number of stops
fewestStops start stop d ss = minimumBy length(filter x(routes start stop ss)) --Filters the list of minimum stops from start to end
                                where x y = allOkay d (pairwise y) --Uses the functions above to compare the integers in the list

-- >>> fewestStops 0 100 25 [5,23,30,36,46,55,62,64,69,70,75]
-- [0,5,30,55,75,100]

-- >>> fewestStops 0 99 24 [5,23,30,36,46,55,62,64,69,70,75]
-- [0,23,36,55,75,99]
