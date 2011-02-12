module Step_3_1 where

import Slides

page = slideDeck "Chapter 3"
    [ titleSlide "Chapter 3"
        [ "Tuples, Types, and Maybe"
        ]
    , codeSlide "Two Things"
        "A 2-Tuple is pair of two values, the fst and the snd \
        \(not a typo!):"
        [ "(43, \"Bob\")     -- a tuple of an Int and String"
        , "(\"Bob\", 43)     -- a tuple of a String and an Int"
        , "(True, [1, 2, 3]) -- a tuple of a Bool and a list"
        , ""
        , "('x', (True, 12)) -- hmmmmm..."
        ]
    , codeSlide "Two Things"
        "When we talk about the type of a tuple, we must specify \
        \the type of the two parts:"
        [ "(Int, String)"
        , "(String, Int)"
        , "(Bool, [Int])"
        , ""
        , "(Char, (Bool, Int))"
        ]
    , codeSlide "Accessing the parts"
        "You can both pattern match, or use functions to access the \
        \parts of a tuple:"
        [ "isAdult (age, name) = age >= 18"
        , ""
        , "isAdult person = fst person >= 18"
        ]
    , codeSlide "Putting them together"
        "You can put them together by using parenthesis and comma:"
        [ "makePerson :: Int -> String -> (Int, String)"
        , "makePerson age name = (age, name)"
        ]
    ]