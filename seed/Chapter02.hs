module Chapter02 where
import Text.Html
-- The lines above are just boilerplate (for now).

page = thehtml <<
    [ header << (thetitle << "Step 2")
    , body <<
        [ h1 << "Groceries"
		, paragraph << "Stuff to get at the store:"
		, ulist << fruit
        ]
    ]

--
-- *** LISTS ***
--
-- This code defines a list:

fruit = [ li << "Apple", li << "Banana", li << "Cherry" ]

-- The square brackets surround a list of values. The values are separated
-- by commas. And no, you can't have an extra comma after the last value.

--
-- *** TRY THIS ***
-- 
-- Add your favorite fruit to the list above. Remove a fruit. Try it with no
-- fruit at all!  Remember that this is a list of <li> elements, and you need
-- the li << code in front of each string to put the string in the <li> element.


-- If your list is longer, or the items wider, you can add more white space
-- and newlines as you see fit. You just need to be sure that all subsequent
-- lines are indented more than the first. The following are all fine
---alternatives for writing the same list. When we write lists over multiple
-- lines, we tend to use the first style.

altFruit1 =
    [ li << "Apple"
    , li << "Banana"
    , li << "Cherry"
    ]

altFruit2 = [
	li << "Apple",
	li << "Banana",
	li << "Cherry" ]

altFruit3 =
	[
		li << "Apple",
		li << "Banana",
		li << "Cherry"
	]

--
-- *** TRY THIS ***
--
-- Make the fruit really long. Here are some ideas to get your taste buds going:
--
-- Daikon, Eggplant, Fennel, Grapes, Honeydew, Icky, Jalapeño, Honeydew,
-- Kale, Lemon, Melon, Nectarine, Orange, Pippin, Quince, Raspberry, Sultana,
-- Tarragon, Ugh, Valentine, Watermelon, Xfruit, Yarrow, Zzzz
--
-- Hungry yet?