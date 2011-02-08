module Chapter03 where
import Text.Html
-- The lines above are just boilerplate (for now).

page = thehtml <<
    [ header << (thetitle << "Step 3")
    , body <<
        [ h1 << "Groceries"
        , paragraph << "Stuff to get at the store:"
        , ulist << fruit
        ]
    ]

--
-- In Haskell, all the items in the list have to be the same type. In this
-- case they are Html fragments, in particular <li> elements.
-- Here are some more:

aListOfStrings = ["astronaut", "baker", "candlestick maker"]
aListOfNumbers = [6, 42, -1, 1000000]
-- aListThatWon'tCompile = ["answer", 42]

--
-- *** TRY THIS ***
--
-- Try editing some string inside double quotes and save it. See what it does.
-- Try adding another paragraph. Just duplicate one of those lines and try it.
-- Add some other common block elements: Try h2 or pre.

--
-- *** A LIST ***
--

-- That defines a new name, fruit, that is an HTML fragment. Try putting that
-- into the list of items in the body of the page. Just put ", fruit" on a line
-- by itself in there, making sure to line the commas up.

-- You can define sections of your own, and add them the same way, such as:

footnote = paragraph << (small << "You are getting sleepy.")

fruit = map (li <<) $ words "apple banana kiki"

