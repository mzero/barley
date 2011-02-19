module Step_2_1 where

import Slides

page = slideDeck "Chapter 2"
    [ titleSlide "Chapter 2"
        [ "HTML Output"
        , "Four ways to write a to-do list."
        ]
    , codeSlide "Text.Html"
        "From now own, we'll be generating HTML output. \
        \We do this with the Text.Html library, which introduces some new functions:"
        [ "h1 h2 h3 p pre  -- most tags are defined as their own names"
        , "italics thediv  -- some have quirky names"
        , "ulist olist     -- more quirky ones"
        ]
    , codeSlide "Text.Html"
        "You put content inside a tag with the << operator:"
        [ "h1 << \"Heading\" -- content can be a String"
        , "blockquote << paragraph << \"blah blah ...\" -- or another tag"
        , "ulist << [ li << \"Apple\", li << \"Banana\", li << \"Kiki\" ]"
        , "    -- or a list of content"
        ]
    , codeSlide "Text.Html"
        "Some other useful functions:"
        [ "toHtml  -- converts strings or lists to Html content"
        , "+++     -- concatenate Html content"
        , "noHtml  -- an empty Html content"
        ]
    , pointSlide "Onward"
        "Now let's build a To-Do list..."
        [ "Be brave"
        , "Read the comments"
        , "Go on to the next tutorial step."
        ]
    ]