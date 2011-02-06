module Step1 where

import Slides

page = slideDeck "Chapter 1: Intro"
    [ titleSlide "Intro to Haskell"
        [ "Chapter 1"
        , "The Barley Project"
        ]
    , pointSlide "Welcome!"
        "This is going to be:"
        [ "Fun!"
        , "Hands on"
        ]
    , pointSlide "What's up with Functional?"
        "What does a program do?"
        [ "It consumes old data."
        , "It computes over the old data."
        , "It produces new data."
        ]
    , pointSlide "A familiar program"
        "Consider the Unix sort command:"
        [ "Consumes (possibly unsorted) lines of text."
        , "Sorts the lines."
        , "Produces sorted lines of text."
        ]
    , pointSlide "What's functional programming?"
        "Functional programming is a style of programming that emphasizes funtions \
        \and function application."
        [ ]
    ]

        