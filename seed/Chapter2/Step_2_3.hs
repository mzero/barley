module Step_2_3 where

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


-- This time we've rewritten the lineNumbers function using "guards":
poem :: String
poem = "`Twas brillig, and the slithy toves\n"
    ++ "Did gyre and gimble in the wabe;\n"
    ++ "All mimsy were the borogoves,\n"
    ++ "And the mome raths outgrabe.\n"

oneNumber :: Int -> String -> String
oneNumber n s = show n ++ ": " ++ s

lineNumbers :: Int -> [String] -> [String]
lineNumbers n xs
    | xs == []  = []
    | otherwise = [oneNumber n (head xs)] ++ lineNumbers (n+1) (tail xs)
-- Notice that instead of a single =, there are now two expressions that have
-- and =, both preceeded by a | and a condition. The first condition that
-- matches will choose the = expression to use. otherwise is just another way
-- of saying True in Haskell.

poemText :: String
poemText = unlines $ lineNumbers 1 $ lines poem

-- NEXT

-- Here's a version of renderToDo written with an if statement.
-- Try converting it to using guards:

toDoItems :: [String] -- a list of strings
toDoItems = ["Pick up avacados", "Make snacks", "Clean house", "Have party"]

renderToDo :: [String] -> [Html]
renderToDo ts = 
    if ts == []
    then []
    else [li << head ts] ++ renderToDo (tail ts)

toDoHtml :: Html
toDoHtml = ulist << renderToDo toDoItems


