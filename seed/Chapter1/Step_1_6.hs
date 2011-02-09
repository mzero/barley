module Step_1_6 where

import Data.List

-- Playing with lists

fruit :: [String]
fruit = ["apple", "bannana", "orange", "plum", "peach", "grape", "fig", "apricot"]

ages :: [Integer]
ages = [42, 0, 18, 6, 22, 72]

shhhh :: [String]
shhhh = []

-- These are all lists. Try making your own, and showing them here:

output :: String
output = show fruit
    ++ "\n" ++ show ages
    ++ "\n" ++ show shhhh
-- Note: output has the type String, and so you need to use the show function to convert any
-- of your lists into a String. There is no automatic type conversion in Haskell at all. One
-- day you'll come to view this as a good thing, when you realize it has saved you from
-- countless hard to find bugs!


-- NEXT

-- Play with the following list functions:
{-
    head :: [a] -> a
    tail :: [a] -> [a]
    init :: [a] -> [a]
    last :: [a] -> a
-}

output1 = show $ tail fruit

-- You also have these functions available:
{-
    (++) :: [a] -> [a]
    length :: [a] -> Int
    reverse :: [a] -> [a]
    sort :: [a] -> [a]
-}

-- And these functions for use with String:
{-
    lines :: String -> [String]
    unlines :: [String] -> String
    words :: String -> [String]
    unwords :: [String] -> String
-}

-- Write some expressions using these functions, and tack them onto output above so you can
-- see 'em.
