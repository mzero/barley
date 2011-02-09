module Step_1_5 where

import Slides

page = slideDeck "Chapter 1: Types"
    [ pointSlide "Types?"
        "Where are all those types? Isn't Haskell strongly, static typed?"
        [ "They were all there!"
        , "Haskell uses \"type inferencing\", meaning it figures them out."
        , "When we add types to the program, it is mostly for the programmers!"
        , "Which, it turns out, is a good idea."
        , "Sometimes you do need types, and the compiler will let you know, strangely."
        ]
    , codeSlide "Simple Types"
        "Types are named with an initial capital letter:"
        [ "String"
        , "Int Integer Float Double"
        , "Bool"
        ]
    , codeSlide "List Types"
        "Put a type in square backets to make a list of them."
        [ "[String] -- a list of strings"
        , "[Integers] -- a list of integers"
        , "[[String]] -- a list of lists of strings!"
        ]
    , codeSlide "Type Declarations"
        "You can tell the compiler, and other programmers, the type of an identifier:"
        [ "name :: String"
        , "name = \"Inigo Montoya\""
        , ""
        , "e :: Double"
        , "e = 2.718281828459045"
        , ""
        , "nameParts :: [String]"
        , "nameParts = words name"
        ]
    , pointSlide "Type Declarations"
        "In none of those cases was the type declaration needed, but they were all \
        \useful in making sure you know what types you're working with."
        [ 
        ]
    , codeSlide "Function Types"
        "Function types are written with arrows:"
        [ "addOne :: Integer -> Integer"
        , "addOne x = x + 1"
        , ""
        , "lines :: String -> [String]"
        , "words :: String -> [String]"
        ]
    , codeSlide "Type variables"
        "Sometimes a function doesn't care what types of list it is working with. \
        \We call these polymorphic functions. In this case, the type is written as \
        \a variable, with a lower case letter:"
        [ "reverse :: [a] -> [a]"
        , "length :: [a] -> Int"
        ]
    , codeSlide "Two or more arguments"
        "Mutliple arguments are written as successive terms with arrows"
        [ "sumSquares :: Integer -> Integer -> Integer"
        , "sumSquares a b = a*a + b*b"
        , ""
        , "(++) :: [a] -> [a] -> [a]"
        ]
     , pointSlide "Types"
        "From now on we'll be putting in type declarations"
        [ "Be brave..."
        , "Think about the types of your data"
        , "There are some very cool types coming your way..."
        ]
    ]

        