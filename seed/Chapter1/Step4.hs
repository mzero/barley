module Step4 where

-- Great! You've run your first Haskell code.

import Data.List
-- This line just makes some utility functions from the Data.List module accessible.

-- Let's write those unix command lines as Haskell code:

input = "`Twas brillig, and the slithy toves\n"
    ++ "Did gyre and gimble in the wabe;\n"
    ++ "All mimsy were the borogoves,\n"
    ++ "And the mome raths outgrabe.\n"

output = output1

output1 = unlines ( sort (lines input))
-- This does just what you think it does!

output2 = unlines ( take 2 ( map reverse ( lines input )))
-- This does just what you think it does!

output3 = show ( length (words input))
-- What does this do?
-- Notice that we had to convert the number result of length to a String with show

output4 = "\t" ++ show ( length (lines input))
    ++ "\t" ++ show ( length (words input))
    ++ "\t" ++ show (length input)
-- how about this?

-- NEXT

-- Most haskell code doesn't have so many parenthesis. Instead, it uses the $ operator like so:

output1' = unlines $ sort $ lines input
output2' = unlines $ take 2 $ map reverse $ lines input
output3' = show $ length $ words input
output4' = "\t" ++ (show $ length $ lines input)
    ++ "\t" ++ (show $ length $ words input)
    ++ "\t" ++ (show $ length input)

-- You can think of $ as parenthesizing everything on the right, even other $ to the right.
