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

module Tutorial(
    isTutorialPage,
    tutorialPrev,
    tutorialNext,
    tutorialModule,
    tutorialOutlineModule,
    ) where

import Control.Monad (join)
import Text.Html

data StepType = Slides | Code
data Step = Step { stepUrl :: String, stepType :: StepType }

stepLink :: Step -> Html
stepLink (Step u t) = anchor ! [href $ "source?file=" ++ u ++ ".hs" ++ p t] << u
  where
    p Code = ""
    p Slides = "&preview=1"

-- | The list of URL paths that are in the tutorial, in order.
steps :: [Step]
steps =
    [ Step "Chapter1/Step_1_1" Slides
    , Step "Chapter1/Step_1_2" Code
    , Step "Chapter1/Step_1_3" Slides
    , Step "Chapter1/Step_1_4" Code
    , Step "Chapter1/Step_1_5" Slides
    , Step "Chapter1/Step_1_6" Code
    , Step "Chapter1/Step_1_7" Code
    , Step "Chapter2/Step_2_1" Slides
    , Step "Chapter2/Step_2_2" Code
    , Step "Chapter2/Step_2_3" Code
    ]

stepUrls :: [String]
stepUrls = map stepUrl steps

-- | A assoc list that maps URLs in the tutorial to the previous and next URLs
-- in the list
pagePrevNext :: [(String, (Maybe Step, Maybe Step))]
pagePrevNext = zip stepUrls $ zip prevList nextList
  where
    prevList = Nothing : justSteps
    nextList = tail justSteps ++ [Nothing]
    justSteps = map Just steps

lookupIn :: (Eq k) => [(k, v)] -> k -> Maybe v
lookupIn = flip lookup


isTutorialPage :: String -> Bool
isTutorialPage t = t `elem` stepUrls

tutorialPrev :: String -> Maybe Step
tutorialPrev = join . fmap fst . lookupIn pagePrevNext

tutorialNext :: String -> Maybe Step
tutorialNext = join . fmap snd  . lookupIn pagePrevNext

tutorialModule :: String -> Maybe Html
tutorialModule t =
    if isTutorialPage t then Just modHtml else Nothing
  where
    modHtml = (h2 << "Tutorial") +++
        [ p << (toHtml "Prev: " +++ linkTo (tutorialPrev t))
        , p << (toHtml "Next: " +++ linkTo (tutorialNext t))
        ]
    linkTo = maybe (toHtml "--none--") stepLink
    
tutorialOutlineModule :: Html
tutorialOutlineModule = (h2 << "Tutorial") +++ unordList (map stepLink steps)
