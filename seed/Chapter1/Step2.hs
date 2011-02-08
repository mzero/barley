-- This is a file of Haskell code. Don't worry about trying to understand all
-- the details of how it works, or the syntax for now. Just follow along and
-- try stuff.

-- Frist thing to know is that these are comments. Everything after two dashes
-- and a space to the end of the line is a comment. In these tutorials you should
-- READ THE COMMENTS, because that is where we guide you through.

module Step2 where
-- The line above just declares the name of this module, which keeps all the
-- names of things defined here separate from things named in other files which
-- are their own modules. This module is named Step2.

output = "Hello Haskell user!"
-- The Barley environment looks for something defined output and renders that String
-- as a text page when fetched by the browser.

-- TRY IT: Press the "Run" switch in the upper right, above this window. When you're
-- done, press the "Edit" switch to come back here.

-- NEXT

-- In Haskell, variables aren't really variable: Their value can only be defined once.
-- To have this module output something else, comment out the definition of output
-- above, and un-comment this one:

-- output = output1

output1 = "The answer: " ++ show (6 * 7)

-- Several things to notice here:  ++ is the concatenation operator, and it works with
-- Strings. show is a function that takes a value an produces a string representation
-- of it. Multiplication is *, just like in every other langauge.

-- NEXT

-- Change the output line above to be output2, and then play around here, concatenating
-- some other things to output2. Consider some of the examples in the comments.

output2 = "Other stuff: "
    -- ++ "Yo!"
    -- ++ show 42
    -- ++ show [ 23, 13, 4, 700, 8 ]
    -- ++ show (sort [ 23, 13, 4, 700, 8 ])

-- You can just remove the leading dashes. In Haskell, expressions can be continued
-- on the next line so long as they are indented from the starting line.

-- NEXT

-- Now click on the next link in the Tutorial section on the right -->