module Tests where

-- GHC
import System.Exit
import System.Environment

-- External
import Test.HUnit
import Data.Char (toUpper)
import Data.List (sort, nub)

-- Lib
import Problem6

--------------------------------------------------------------------------------
-- Part 1
--------------------------------------------------------------------------------

p61 :: Test
p61 = test $ [
  routes 0 1 []             @?= [[0, 1]],
  sort (routes 0 10 [2, 3]) @?= [[0,2,3,10],[0,2,10],[0,3,10],[0,10]],
  sort (routes 5 10 [6, 7, 8]) @?= [[5,6,7,8,10],[5,6,7,10],[5,6,8,10],[5,6,10],[5,7,8,10],[5,7,10],[5,8,10],[5,10]],
  ([50, 51, 52, 53, 54, 55] `elem` (routes 50 55 [51, 52, 53, 54])) @? "routes contains full enumeration"
  ]
  ++  [ ([100, x, 110] `elem` (routes 100 110 [101..109]))
        @? "routes 100 110 [101..109] contains [100, " ++ show x ++ ", 110]" | x <- [101..109] ]

--------------------------------------------------------------------------------
-- Part 2
--------------------------------------------------------------------------------
  
p62 :: Test
p62 = test [
  pairwise [1, 2] @?= [(1, 2)],
  let xs = [1..25] in pairwise xs @?= [(x, y) | x <- xs, y <- xs, y - x == 1 ],
  let xs = [20, 18..0] in pairwise xs @?= [(y, x) | x <- xs, y <- xs, y - x == 2],
  pairwise [5, 42, 33, 111] @?= [(5, 42), (42, 33), (33, 111)],
  -- allOkay
  allOkay 2 (pairwise [2,4..40]) @?= True,
  allOkay 3 (pairwise [3,7..43]) @?= False,
  allOkay 1 (pairwise [1..27]) @?= True,
  allOkay 9 (pairwise [10,20..100]) @?= False
  ]

--------------------------------------------------------------------------------
-- Part 3
--------------------------------------------------------------------------------

next :: Int -> Int
next x | even x    = x `div` 2
       | otherwise = 3 * x + 1

p63 :: Test
p63 = test [
  minimumBy show [1, 2, 3, 4] @?= 1,
  minimumBy next [50,45..5] @?= 10,
  minimumBy (\x -> -x) [1..25] @?= 25,
  minimumBy length (words "This sentence has no word smaller than two characters")  @?= "no",
  minimumBy id [1] @?= 1
  ]
  
--------------------------------------------------------------------------------
-- Part 4
--------------------------------------------------------------------------------

stopsShort :: [Int]
stopsShort = [22,92,123,147,187,220,251,257,276,282]
                   
p64 :: Test
p64 = test [
  fewestStops 0 10 5 [1..5] @?= [0, 5, 10],
  fewestStops 0 1  1 []     @?= [0, 1],
  fewestStops 0 100 25 [25, 50, 75, 80] @?= [0,25,50,75,100],
  length (fewestStops 0 300 100 stopsShort) <= 5 @? "Stops shouldn't exceed 5"
  ]
  

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

argMap :: Int -> Test
argMap 1 = p61
argMap 2 = p62
argMap 3 = p63
argMap 4 = p64
argMap _ = test [p61, p62, p63, p64]

hd :: [a] -> Maybe a
hd (x : xs) = Just x
hd []       = Nothing

main :: IO ()
main = do
  args <- getArgs
  let
    tests = case read <$> (hd args) of
          Just x -> argMap x
          Nothing -> argMap 42
  results <- runTestTT tests
  if (errors results + failures results == 0)
    then
      exitWith ExitSuccess
    else
      exitWith (ExitFailure 1)
