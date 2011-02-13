module Step_1_3 where

import Slides

page = slideDeck "Chapter 1: Syntax"
    [ pointSlide "Syntax"
        "Haskell is not that different than most languages:"
        [ "Operators work as you might expect: 2 + 3 * 4 is 14"
        , "Use parentheses to group terms: (2 + 3) * 4 is 20"
        , "It uses indentation and ';' similarly to Python"
        , "The = sign sets the value of an identifier"
        , "String literals are in double quotes with usual escapes."
        ]
    , pointSlide "Functional Application"
        "But the one big difference is in Function application:"
        [ "There are no parenthesis around the arguments."
        , "There are no commas between the arguments."
        ]
    , codeSlide "Function Application"
        "Just drop the parenthesis..."
        [ "length(s) // as in C*/P*/J*"
        , ""
        , "length s  -- Haskell"
        ]
    , codeSlide "Function Application"
        "Just drop the commas..."
        [ "lookup(key, dict) // as in C*/P*/J*"
        , ""
        , "lookup key dict   -- Haskell"
        ]
    , codeSlide "Function Application"
        "Function call still binds tighter than operators"
        [ "lookup(key, dict) + 1 // as in C*/P*/J*"
        , ""
        , "lookup key dict + 1   -- Haskell"
        ]
    , codeSlide "Function Application"
        "But arguments that are expressions will need parenthesis"
        [ "lookup(key + 1, dict) // as in C*/P*/J*"
        , ""
        , "lookup (key + 1) dict -- Haskell"
        ]
    , codeSlide "Function Definition"
        "This is similar in function definitions, too:"
        [ "f(a,b) = { return 2*a + 3*b; } // as in C*/J*"
        , ""
        , "f a b = 2*a + 3*b              -- Haskell"
        ]
     , pointSlide "Syntax"
        "It's been described as \"crazy moon language\", but..."
        [ "Be brave..."
        , "You'll soon be able to read it easily"
        , "You'll soon love how clear and consise it is"
        , "You'll soon loath to type in those other, verbose languages."
        ]
    ]

        