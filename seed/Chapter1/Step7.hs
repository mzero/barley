module Step7 where

-- Time to write your our first function

input :: String
input = "`Twas brillig, and the slithy toves\n"
    ++ "Did gyre and gimble in the wabe;\n"
    ++ "All mimsy were the borogoves,\n"
    ++ "And the mome raths outgrabe.\n"

output :: String
output = output2

oneNumber :: Int -> String -> String
oneNumber n s = show n ++ ": " ++ s

output1 :: String
output1 = oneNumber 3 "All mimsy..."


lineNumbers :: Int -> [String] -> [String]
lineNumbers n xs =
    if xs == []
    then []
    else [oneNumber n (head xs)] ++ lineNumbers (n+1) (tail xs)


output2 :: String
output2 = unlines $ lineNumbers 1 $ lines input

