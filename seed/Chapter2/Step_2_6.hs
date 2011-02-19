module Step_2_6 where

import Text.Html

page = thehtml <<
    [ header << (thetitle << "Output")
    , body <<
        [ h1 << "A to-do list:"
        , thediv << toDoHtml
        , h1 << "Some action:"
        , thediv << comicAction
        ]
    ]


-- Here's a version of renderToDo written with patterns.
-- Getting simpler, isn't it?

toDoItems :: [String] -- a list of strings
toDoItems = ["Pick up avocados", "Make snacks", "Clean house", "Have party"]

renderToDo :: [String] -> [Html]
renderToDo []     = []
renderToDo (t:ts) = li << t : renderToDo ts

toDoHtml :: Html
toDoHtml = ulist << renderToDo toDoItems

-- Here's another kind of list

comicbookWords :: [String]
comicbookWords = ["Pow", "Blamo", "Zap"]

comicStyle :: String -> Html
comicStyle word = paragraph << bold << ("** " ++ word ++ "! **")

renderAction :: [String] -> [Html]
renderAction []     = []
renderAction (w:ws) = comicStyle w : renderAction ws

comicAction :: Html
comicAction = blockquote << renderAction comicbookWords

-- Look carefully at renderAction and renderToDo. Do they look similar?

-- Each is applying some operation to the first element of the list, and
-- constructing a list from the result and a recursive call over the remainder
-- of the list. When there are no more elements, they return an empty list.

-- Haskellers hate repeating themselves!

-- This pattern is so common there a higher-order function to do this: map

renderToDo' :: [String] -> [Html]
renderToDo' ts = map (li <<) ts

renderAction' :: [String] -> [Html]
renderAction' ws = map comicStyle ws

-- Try changing toDoHtml and comicAction to use these versions and see that they
-- work the same.

-- Oh yeah, we slipped in two more things about Haskell there.
-- 1) You can use ' as a character in names
-- 2) (li <<) is a handy shortcut. It makes a function that takes an argument
--    and applies to the empty side of the operator. You can do this with any
--    operator(*):

addTwo = (+ 2)
dividedByTwo = (/ 2)
twoOver = (2 /)
putStarsInFront = ("**" ++)
putStarsAtEnd = (++ "**")


-- map is so useful, and makes your code very short. Even more amazing is that
-- there nothing special about the implementation of map. In fact you can easily
-- make your own!

-- The hardest part of myMap is the type signature, so we'll give you that:

myMap :: (a -> b) -> [a] -> [b]

-- Let's break this down:
-- (a -> b) -- the first argument is a function! this function takes any type
--             of argument, let's call it a, and produces some type of result,
--             let's call it b
-- -> [a]   -- the second argument to map is a list of somethings, and they'd
--             better be the same type as the argument to the function argument
-- -> [b]   -- the result is a list of somethings, and they'll be the same type
--             as the function's result

-- Notice that the type signature didn't care about particular types, so it used
-- type variables to talk about them.

-- Now, here's the first clause of myMay. You can write the other:

myMap f []     = []

-- Replace map in the above functions with myMap and see that it works.


--------------
-- (*) Except - ! You can't because (-3) means negative 3.
