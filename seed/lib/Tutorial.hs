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
    tutorialModule
    ) where

import Control.Monad (join)
import Text.Html

-- | The list of URL paths that are in the tutorial, in order.
pageList :: [String]
pageList =
    [ "Chapter01"
    , "Chapter02"
    , "Chapter03"
    ]

-- | A assoc list that maps URLs in the tutorial to the previous and next URLs
-- in the list
pagePrevNext :: [(String, (Maybe String, Maybe String))]
pagePrevNext = zip pageList $ zip prevList nextList
  where
    prevList = Nothing : justPageList
    nextList = tail justPageList ++ [Nothing]
    justPageList = map Just pageList

lookupIn :: (Eq k) => [(k, v)] -> k -> Maybe v
lookupIn = flip lookup


isTutorialPage :: String -> Bool
isTutorialPage t = t `elem` pageList

tutorialPrev :: String -> Maybe String
tutorialPrev = join . fmap fst . lookupIn pagePrevNext

tutorialNext :: String -> Maybe String
tutorialNext = join . fmap snd  . lookupIn pagePrevNext

tutorialModule :: (String -> String) -> String -> Maybe Html
tutorialModule mkLink t =
    if isTutorialPage t then Just modHtml else Nothing
  where
    modHtml = (h2 << "Tutorial") +++
        [ p << (toHtml "Prev: " +++ linkTo (tutorialPrev t))
        , p << (toHtml "Next: " +++ linkTo (tutorialNext t))
        ]
    linkTo = maybe (toHtml "--none--") asLink
    asLink u = anchor ! [href $ mkLink u] << u
    
