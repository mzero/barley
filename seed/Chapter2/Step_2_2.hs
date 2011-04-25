module Step_2_2 where

import Text.Html

page = thehtml <<
    [ header << (thetitle << "Output")
    , body <<
        [ h1 << "A poem from last time:"
        , pre << poemText
        , h1 << "A to-do list:"
        , thediv << toDoHtml
        ]
    ]


-- Here's our line number function from last time:
poem :: String
poem = "`Twas brillig, and the slithy toves\n"
    ++ "Did gyre and gimble in the wabe;\n"
    ++ "All mimsy were the borogoves,\n"
    ++ "And the mome raths outgrabe.\n"

oneNumber :: Int -> String -> String
oneNumber n s = show n ++ ": " ++ s

lineNumbers :: Int -> [String] -> [String]
lineNumbers n xs =
    if xs == []
    then []
    else [oneNumber n (head xs)] ++ lineNumbers (n+1) (tail xs)

poemText :: String
poemText = unlines $ lineNumbers 1 $ lines poem

-- NEXT

-- Here's a simple to-do list, and some code that converts it into Html
-- It doesn't do a very nice job. Which you can see if you hit Run.

toDoItems :: [String] -- a list of strings
toDoItems = ["Pick up avocados", "Make snacks", "Clean house", "Have party"]

renderToDo :: [String] -> [Html]
renderToDo ts = [toHtml $ show ts]

toDoHtml :: [Html]
toDoHtml = renderToDo toDoItems

-- Rewrite the function renderToDo so that it builds up a ul element with 
-- list of li elements as the content. You may want to make a helper function
-- like oneNumber to make the individual li elements.

