-- Copyright 2010 Google Inc.
-- 
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
-- 
--      http://www.apache.org/licenses/LICENSE-2.0
-- 
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module Index where

import DevUtils
import Tutorial
import Text.Html

page = devpage "Start"
        [ h1 << "Welcome aboard!"
        , p << "Barley is an environment and tutorial for exploring Haskell. \
             \Our aim is to make your first encounter with Haskell fun, \
             \enjoyable, and practical. "
        , steps
        ]
        [ modMessage, tutorialOutlineModule, modTagLine ]
        []


type Step = (String, [Html])

steps :: Html
steps = ordList $ map mkStep [step1, step2, step3, step4]
  where
    mkStep (title, text) = p << bold << title +++ thediv << text
    
step1 :: Step
step1 = ("Install and run Barley",
  [ p << "If you got here, you've done this step!"
  ])

step2 :: Step
step2 = ("Navigating Barley",
  [ p << "This web site is a small development envinronment for Haskell. \
         \Here are the main navigation elements:"
  , p << (bold << "Top bar:" +++ "The black top bar at the very top of \
         \the page has links to commonly used pages. At the top right, \
         \Home takes you back to this page; Project takes you to a listing \
         \of all the files that make up Barley. Documentation and Help are \
         \what they say they are. In the upper left, the Haskell icon leads \
         \to some common community web pages.")
  , p << (bold << "Right sidebar:" +++ "On the right there are useful operations. \
         \On this page, it includes \
         \a listing of the complete tutorial. On other pages there are actions \
         \relevant to the page you are on. If you are on a tutorial page, that \
         \is where the next and previous steps will be listed.")
  ])

step3 :: Step
step3 = ("A Sample Page",
  [ p << "If you want to try something right now, do this: "
  , unordList
      [ toHtml "Be sure you know how to get back here: The Home link in the upper right."
      , toHtml "Be sure to read all the comments in the code you are about to see. \
               \This is where the instructions will be."
      , toHtml "Be brave."
      , toHtml "Now go to " +++ anchor ! [href "source?file=FirstPage.hs"] << "FirstPage.hs"
      , toHtml "Come back here when you're done."
      ]
  , p << "Congratulations! You're writing Haskell!"
  ])

step4 :: Step
step4 = ("Do the Tutorial",
  [ p << "The tutorial is a sequence of pages written in Barley. Some pages are \
          \a sequence of slides, others are code that you will edit and work on."
  , p << "On the code pages, you should be sure to read the comments in the code, \
          \since that is where the tutorial introduces new ideas, and guides you \
          \along. On these pages you'll see a big Edit/Run switch near the top of \
          \the page that will let you switch back and forth between editing the code \
          \and viewing the page that that code renders."
  , p << ("Start here: " 
          +++ anchor ! [href "source?file=Chapter1/Step_1_1.hs&preview=1"] << "Chapter 1, Step 1")
  ])
  
modMessage :: Html
modMessage = (h2 << "pre-Alpha version") +++
    (p << ("Just in case it wasn't totally clear: This is very early, \
          \pre-Alpha software. We're making it up as fast as we can! \
          \In the spirit of open source, we are developing in public, \
          \so please be gentle!" +++ br +++ "— Johan & Mark"))

modSteps :: Html
modSteps = (h2 << "Tutorial Steps") +++
    ordList [ "Step", "Step", "Quick-Step", "Slide" ] +++
    p << "This will be a list of the steps some day…"

modTagLine :: Html
modTagLine = pre << (
    "webSite :: " +++ (bold << "Haskell") +++ "\n\
    \webSite = madeWith\n\
    \  [ haskellPlatform\n\
    \  , snapFramework\n\
    \  , plugins\n\
    \  , ghc\n\
    \  ]"
    )


