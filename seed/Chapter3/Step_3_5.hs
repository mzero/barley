module Step_3_5 where

import Slides

page = slideDeck "Chapter 3"
    [ codeSlide "Not that safe"
        "Type synonyms are quick and easy and often the right choice, but \
        \don't offer as much type saftey as possible. Consider:"
        [ "type Range = (Int, Int)"
        , "inRange :: Range -> Int -> Bool"
        , "inRange (lo,hi) x = lo <= x && x <= hi"
        , ""
        , "type Point = (Int, Int)"
        , "p :: Point"
        , "p = (3,7)"
        , ""
        , "ick = inRange p 4 -- compiles!"
        ]
    , codeSlide "Data Types"
        "Data types let you create a type that is distinct. When you so you \
        \define a constructor that builds values of the type:"
        [ "data Range = Range Int Int"
        , "inRange :: Range -> Int -> Bool"
        , "inRange (Range lo hi) x = lo <= x && x <= hi"
        , ""
        , "data Point = Point Int Int"
        , "p :: Point"
        , "p = Point 3 7"
        , ""
        , "ick = inRange p 4 -- now this is a compile error"
        ]
    , codeSlide "Multiple Constrcutors"
        "Data types are very powerful. They can have more than one constructor \
        \and each constructor can have any number of values:"
        [ "data Direction = North | East | South | West"
        , "  deriving (Eq, Show)"
        , ""
        , "data Weather = Sunny | Rainy Double | Windy Double Direction"
        , "describe :: Weather -> String"
        , "describe Sunny = \"lovely\""
        , "describe (Rainy inches) | inches < 0.2 = \"drizzle\""
        , "                        | otherwise    = \"wet\""
        , "describe (Windy speed dir) = "
        , "    \"blowin' \" ++ show speed ++ \" from \" ++ show dir"
        ]
    , codeSlide "Wait, Deriving What?"
        "The deriving clause on the data declaration is a bit of boilerplate \
        \saver that will make your data do more. Here are some of the things \
        \you can 'derive' for your data automatically:"
        [ "Eq      -- works with == and /= operators"
        , "Ord     -- works with <, >, etc.."
        , "Show    -- works with show"
        , "Read    -- works with read"
        , "Enum    -- convertable to/from integers"
        , "Bounded -- has a min and max value"
        ]
    ]