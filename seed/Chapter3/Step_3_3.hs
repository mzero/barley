module Step_3_3 where

import Slides

page = slideDeck "Chapter 3"
    [ codeSlide "Bigger Tuples"
        "Tuples can be larger: 3 or more elements. Here are some larger tuple \
        \types:"
        [ "(Int, String, Bool)"
        , "(Int, Int, Int, Int) -- you can use the same type more than once"
        , "(Bool, Bool, Bool, Bool,"
        , " Bool, Bool, Bool, Bool) -- a byte? please, no!"
        ]
    , codeSlide "Bigger Tuples"
        "There aren't any accessor functions defined for these, so you have \
        \to pattern match to get their parts. Though you could define:"
        [ "fst3 :: (a, b, c) -> a"
        , "fst3 (a, b, c) = a"
        , ""
        , "snd3 :: (a, b, c) -> b"
        , "snd3 (a, b, c) = b"
        , ""
        , "trd3 :: (a, b, c) -> c"
        , "trd3 (a, b, c) = c"
        ]
    , pointSlide "Lists vs. Tuples"
        "Lists and Tuples are different:"
        [ "In a list the elements are all the same type."
        , "In a tuple members can be different types."
        , ""
        , "A list can have any number of elements."
        , "A tuple always has the number of members indicated by its type."
        ]
    , codeSlide "Type names"
        "You can give a name to a type. This makes things more convient to \
        \use:"
        [ "type Person = (Int, String)"
        , ""
        , "isAdult :: Person -> Bool"
        , "isAdult (age,name) = age >= 18"
        ]
    , codeSlide "Type names"
        "Watch out: The type is just a synonym. amy and bob are still the \
        \same type:"
        [ "type Person = (Int, String)"
        , ""
        , "amy :: Person"
        , "amy = (28, \"Amy\""
        , ""
        , "bob :: (Int, String)"
        , "bob = (26, \"Bob\""
        ]
    ]