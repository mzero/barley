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

module Documentation where

import DevUtils

import Control.Monad (liftM)
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Snap.Types
import Text.Html

handler :: Snap ()
handler = do
    docparam <- getParam (C.pack "doc")
    let docid = fromMaybe defaultdoc (fmap C.unpack docparam >>= finddoc)
    htmlResponse $ docpage docid

docpage :: Doc -> Html
docpage doc = devpage "Documentation"
    [ tag "iframe" ! [ src (docUrl doc), identifier "documentation" ] << noHtml
    ]
    [] -- modules
    [] -- scripts

finddoc :: String -> Maybe Doc
finddoc x = go documents x
  where
    go (d:ds) x | docId d == x = Just d
                | otherwise    = go ds x
    go [] _ = Nothing

defaultdoc :: Doc
defaultdoc = head documents
