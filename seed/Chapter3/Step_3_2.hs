module Step_3_2 where

import Text.Html

page = thehtml <<
    [ header << (thetitle << "Output")
    , body <<
        [ h1 << "A to do list:"
        , thediv << toDoHtml
        ]
    ]

-- We've changed the type of toDoItems to include a Bool value to
-- indicate if they've been done.

toDoItems :: [(Bool, String)] -- a list of tuples: each a Bool and String
toDoItems =
    [ (True,  "Pick up avacados")
    , (True,  "Make snacks")
    , (False, "Clean house")
    , (False, "Have party")
    ]
    -- Haskellers often format lists with long items in this funny way!
    
renderToDo :: [String] -> [Html]
renderToDo ts = map (li <<) ts

toDoHtml :: Html
toDoHtml = ulist << renderToDo toDoItems

-- Try running this file. You'll find that the it doesn't compile, and gives
-- an error on the line above. Just read the first two lines of the error and
-- see if you can see what the compiler is trying to tell you about the problem
-- with the types.

-- Fix the renderToDo function to fix the problem.

-- NEXT

-- What did you end up doing with the Bool component of the tuple? Did you
-- ignore it? Did you use it in the computation somehow? Did you notice that
-- by having a clear type, you were forced to think about it?

-- Change the renderToDo function to render the items that aren't done bold.

-- NEXT

-- For a challenge, change the renderToDo function to render only the first
-- not done item bold.

hint1 = map pred "Zpv!qspcbcmz!ibwf!up!hp!cbdl!up!opu!vtjoh!nbq/"
hint2 = map pred "Zpv!qspcbcmz!xjmm!offe!bopuifs!ifmqfs!gvodujpo/"