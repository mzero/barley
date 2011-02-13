module Step_3_4 where

import Text.Html

page = thehtml <<
    [ header << (thetitle << "Output")
    , body <<
        [ h1 << "A to-do list:"
        , thediv << toDoHtml
        ]
    ]

-- Rewrite all of this to use a type synonym for the tuple.

toDoItems :: [(Bool, String)] -- a list of tuples: each a Bool and String
toDoItems =
    [ (True,  "Pick up avocados")
    , (True,  "Make snacks")
    , (False, "Clean house")
    , (False, "Have party")
    ]

formatToDo :: (Bool, String) -> Html
formatToDo (True,item) = li << item
formatToDo (False,item) = li << bold << item

renderToDo :: [(Bool, String)] -> [Html]
renderToDo ts = map formatToDo ts

toDoHtml :: Html
toDoHtml = ulist << renderToDo toDoItems

