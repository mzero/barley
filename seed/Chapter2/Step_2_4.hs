module Step_2_4 where

import Slides

page = slideDeck "Chapter 2: Lists"
    [ titleSlide "All About Lists"
        [ ]
    , codeSlide "Making Lists"
        "Up to now we've been making lists using this syntax:"
        [ "[]        -- the empty list"
        , "[42]      -- a list of one value"
        , "[1, 2, 3] -- a list of three values"
        ]
    , pointSlide "The Truth About Lists"
        "Lists in Haskell are actually linked lists, not arrays, and are \
        \built recursively. A list is always just one of these two forms:"
        [ "The empty list"
        , "The first item in the list, and the rest of the list"
        ]
    , codeSlide "Lists in Haskell"
        "In Haskell, [] is the empty list, and the : operator is used to build\
         \up lists:"
        [ "[]             -- the empty list"
        , "42 : []        -- a list of one value"
        , "1 : 2 : 3 : [] -- a list of three values"
        ]
    , codeSlide "Lists in Lisps"
        "In various Lisp dialects, this would look like:"
        [ "nil                           -- the empty list"
        , "(cons 42 nil)                 -- a list of one value"
        , "(cons 1 (cons 2 (cons 3 []))) -- a list of three values"
        ]
    , codeSlide "Syntatic Sugar"
        "In Haskell, the way we've been writing lists is just syntatic sugar \
        \for their construction with the : operator and []:"
        [ "[42]      == 42 : []"
        , "[1, 2, 3] ==  1 : 2 : 3 : []"
        ]
    , codeSlide "Inspecting Lists"
       "When working with lists, so far we've used functions and guards. \
       \Consider:"
       [ "showFirstNumber :: [Int] -> String"
       , "showFirstNumber is ="
       , "    | is == [] = \"No first number.\""
       , "    | otherwise = \"The first number is \" ++ show (head is) ++ \".\""
       ]
    , codeSlide "Inspecting Lists"
       "Instead, since there are just two forms of list, and those are exactly \
       \what this function is looking for, we can pattern match for them:"
       [ "showFirstNumber :: [Int] -> String"
       , "showFirstNumber []     = \"No first number.\""
       , "showFirstNumber (i:is) = \"The first number is \" ++ show i ++ \".\""
       ]
    , pointSlide "How's That Work?"
       "Haskell will pick the first 'equation' for a function where the \
       \arguments match the given pattern:"
       [ "[] will match only the empty list"
       , "(i:is) will match a list that has a first item (i) and a rest of the \
           \list (is). Careful: Notice that those are parenthesis, not square \
           \brackets around the : expression!"
       ]
    , codeSlide "Some gotchas:"
       "Look at this function to see how NOT to get caught up between matching \
       \the : operator, vs. matching the syntatic sugar lists:"
       [ "funnyFunction :: [Int] -> String"
       , "funnyFunction [] = \"Empty List\""
       , "funnyFunction [i] = \"One element list\""
       , "funnyFunction (i:is) = \"List with at least one element\""
       , "funnyFunction [i,j] = \"Two element list\""
       , "  -- would never match because the clause above matches two element lists as well!"
       , "funnyFunction [i:is] = \"A list with one element that is at least a one element list\""
       , "  -- the last one won't compile because it doesn't match the type of the argument"
       ]
    , pointSlide "Patterns Ahoy!"
       "Using pattern matching is generally more concise and clear (once you \
       \get used to it)."
       [ "Be brave!"
       , "Write with patterns from now on if you can."
       ]
    ]