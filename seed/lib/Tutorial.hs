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
import Data.List (groupBy)
import Text.Html

data StepType = Slides | Code
data Step = Step { stepCh, stepSt :: Int, stepType :: StepType }

stepUrl :: Step -> String
stepUrl s = "Chapter" ++ cn ++ "/Step_" ++ cn ++ "_" ++ sh
  where
   cn = show $ stepCh s
   sh = show $ stepSt s

stepHref :: Step -> String
stepHref s = "source?file=" ++ stepUrl s ++ ".hs" ++ p (stepType s)
  where
    p Code = ""
    p Slides = "&preview=1"

stepLongName :: Step -> String
stepLongName (Step ch st _) = "Chapter " ++ show ch ++ ", Step " ++ show st

stepShortName :: Step -> String
stepShortName (Step _ st _ ) = "Step " ++ show st

stepLink :: Step -> Html
stepLink s = anchor ! [href $ stepHref s] << stepLongName s


-- | The list of URL paths that are in the tutorial, in order.
steps :: [Step]
steps =
    [ Step 1 1 Slides
    , Step 1 2 Code
    , Step 1 3 Slides
    , Step 1 4 Code
    , Step 1 5 Slides
    , Step 1 6 Code
    , Step 1 7 Code
    , Step 2 1 Slides
    , Step 2 2 Code
    , Step 2 3 Code
    , Step 2 4 Slides
    , Step 2 5 Code
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
tutorialOutlineModule = (h2 << "Tutorial") +++ map section groupedSteps
  where
    groupedSteps = groupBy (\a b -> stepCh a == stepCh b) steps
    section ss@((Step ch _ _):_) = heading +++ list
      where
        heading = p << ("Chapter " ++ show ch)
        list = unordList (map stepLink ss)
        stepLink s = anchor ! [href $ stepHref s] << stepShortName s

