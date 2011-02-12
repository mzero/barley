module Step_3_6 where

import Text.Html

page = thehtml <<
    [ header << (thetitle << "Output")
    , body <<
        [ h1 << "A to do list:"
        , thediv << toDoHtml
        , h1 << "# people invited:"
        , p << show (headcount invited)
        ]
    ]

-- We've written two lists.

data ToDo = ToDo String | Done String

toDoItems :: [ToDo]
toDoItems =
    [ Done "Pick up avacados"
    , Done "Make snacks"
    , ToDo "Clean house"
    , ToDo "Have party"
    ]


data Invite = Invite String Int

who :: Invite -> String
who (Invite name count) = name

invited :: [Invite]
invited =
    [ Invite "Amy" 1
    , Invite "Bob" 7  -- he's got lots of kids
    , Invite "Cam" 2
    , Invite "Doris" 3
    , Invite "Edgar" 2
    , Invite "Fran" 2
    ]
    
formatToDo :: ToDo -> Html
-- Try writing this

renderToDo :: [ToDo] -> [Html]
renderToDo ts = map formatToDo ts

toDoHtml :: Html
toDoHtml = ulist << renderToDo toDoItems

-- NEXT

-- Write this function

headcount :: [Invite] -> Int
headcount is = 0 -- this list was just a place holder, delete it


