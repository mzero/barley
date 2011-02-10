module Step_2_5 where

import Text.Html

page = thehtml <<
    [ header << (thetitle << "Output")
    , body <<
        [ h1 << "A poem from last time:"
        , pre << poemText
        , h1 << "A to do list:"
        , thediv << toDoHtml
        ]
    ]


-- This time we've rewritten the lineNumbers function using "pattern matching":
poem :: String
poem = "`Twas brillig, and the slithy toves\n"
    ++ "Did gyre and gimble in the wabe;\n"
    ++ "All mimsy were the borogoves,\n"
    ++ "And the mome raths outgrabe.\n"

oneNumber :: Int -> String -> String
oneNumber n s = show n ++ ": " ++ s

lineNumbers :: Int -> [String] -> [String]
lineNumbers n []     = []
lineNumbers n (x:xs) = oneNumber n x : lineNumbers (n+1) xs
-- Notice that instead of a single equation (with the function name) =, there
-- are now two. Each repeats the arguments, but with patterns. Like guards, the
-- first equaltion where all the patterns match will be used.

-- Notice something else: The last expression changed:
oldWay n (x:xs) = [oneNumber n x] ++ lineNumbers (n+1) xs
newWay n (x:xs) =  oneNumber n x  :  lineNumbers (n+1) xs
-- Rather than build a one element list (in the brackets) and concatentate (++)
-- it with the rest of the processing, the newWay just constructs (:) the new
-- result list with the element, and the list that is the recursive result.

poemText :: String
poemText = unlines $ lineNumbers 1 $ lines poem

-- NEXT

-- Here's a version of renderToDo written with guards.
-- Try converting it to using patterns:

toDoItems :: [String] -- a list of strings
toDoItems = ["Pick up avacados", "Make snacks", "Clean house", "Have party"]

renderToDo :: [String] -> [Html]
renderToDo ts
    | ts == []  = []
    | otherwise = [li << head ts] ++ renderToDo (tail ts)

toDoHtml :: Html
toDoHtml = ulist << renderToDo toDoItems


