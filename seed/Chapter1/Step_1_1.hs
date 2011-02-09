module Step_1_1 where

import Slides

page = slideDeck "Chapter 1: Intro"
    [ titleSlide "Chapter 1"
        [ "What's all this about Haskell?"
        , "Getting your feet wet."
        ]
    , pointSlide "Haskell"
        "Haskell seems scary 'cause:"
        [ "Oh noes! Where's my state?"
        , "Hey, I don't want my program to be lazy!"
        , "Yo, PHP doesn't need no templates or combintors..."
        , "Uhm, I thought dynamic languages were better?"
        , "MONADS!"
        ]
    , pointSlide "Haskell"
        "But these are cool tools:"
        [ "Functional."
        , "Lazy"
        , "Higher order Functions"
        , "Static Types"
        , "...shhhh: monads."
        ]
    , pointSlide "Haskell"
        "This is what got us hooked:"
        [ "It is a new way to thinking about programming"
        , "It twists the brain in delightful way."
        , "It is very expressive, yet concise and clear"
        , "It is beautiful."
        ]
    , codeSlide "Some familiar things"
        "Consider these unix shell commands:"
        [ "cat foo | sort"
        , ""
        , "cat foo | rev | head"
        ]
    , pointSlide "These are functional"
        "What they do:"
        [ "Take input"
        , "Process the input."
        , "Produce produce output."
        , "Produce output as soon as they're able."
        , "Don't modify any state."
        , "In short, they are functional, pure, and lazy." 
        ]
     , pointSlide "Onward!"
        "Now you're going to dive into actual coding."
        [ "Be brave..."
        , "Don't let the error messages throw you."
        , "Just try it."
        ]
    ]

        