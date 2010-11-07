module Documentation where

import DevUtils

import Control.Monad (liftM)
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Snap.Types
import Text.Html

nu = () -- DO NOT DELETE THIS

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
