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

data Doc = Doc { docId :: String, docName :: String, docUrl :: String }

documents :: [Doc]
documents =
    [ Doc "ghclibs" "Library" "http://www.haskell.org/ghc/docs/6.12.2/html/libraries/frames.html"
    , Doc "html"    "Text.Html" "http://hackage.haskell.org/packages/archive/html/1.0.1.2/doc/html/Text-Html.html"
    , Doc "snap"    "Snap"    "http://snapframework.com/docs/latest/snap-core/index.html"
    ]

finddoc :: String -> Maybe Doc
finddoc x = go documents x
  where
    go (d:ds) x | docId d == x = Just d
                | otherwise    = go ds x
    go [] _ = Nothing

defaultdoc :: Doc
defaultdoc = head documents
