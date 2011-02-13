module Step_3_7 where

import Text.Html

page = thehtml <<
    [ header << (thetitle << "Output")
    , body <<
        [ h1 << "A to-do list:"
        , thediv << toDoHtml
        , reminderHtml
        , h1 << "# people invited:"
        , p << show (headcount invited)
        ]
    ]

data ToDo = ToDo String | Done String

toDoItems :: [ToDo]
toDoItems =
    [ Done "Pick up avocados"
    , Done "Make snacks"
    , ToDo "Clean house"
    , ToDo "Have party"
    ]

-- We need a function to return the next item to do.
-- Something like:
--        whatToDoNext :: [ToDo] -> ToDo

-- But what should it return if they are all done?
-- Enter Maybe:
whatToDoNext :: [ToDo] -> Maybe String

-- The return type is Maybe String -- in the same way [String] is a type 'built'
-- from the String type, Maybe String is a type 'built' from the String type. But
-- rather than a list, values of type Maybe String can have one of two forms:
--         Just someString
--         Nothing
whatToDoNext (Done item : ts) = whatToDoNext ts
whatToDoNext (ToDo item : ts) = Just item
whatToDoNext []               = Nothing

formatNotice :: Maybe String -> Html
formatNotice Nothing = noHtml
formatNotice (Just item) = h1 << ("*** DO THIS: " ++ item ++ " ***")

reminderHtml :: Html
reminderHtml = formatNotice $ whatToDoNext toDoItems

    
formatToDo :: ToDo -> Html
formatToDo (ToDo item) = li << bold << item
formatToDo (Done item) = li << item

renderToDo :: [ToDo] -> [Html]
renderToDo ts = map formatToDo ts

toDoHtml :: Html
toDoHtml = ulist << renderToDo toDoItems

-- Try changing the toDoItems to be all Done and see how this works


-- NEXT
-- Add an optional partner or spouse to the invite data type.
-- Add a list of invitees (and optional partners!) to the page.

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

headcount :: [Invite] -> Int
headcount [] = 0
headcount (Invite who count : is) = count + headcount is

