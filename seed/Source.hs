module Source where

import DevUtils

import Control.Monad (when)
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Snap.Types
import Text.Html hiding ((</>))
import qualified Text.Html as Html

nu = () -- DO NOT DELETE THIS

handler :: Snap ()
handler = do
    fileparam <- getParam (C.pack "file")
    case maybe Nothing (legalPath . C.unpack) fileparam of
        Nothing -> errorBadRequest
        Just file -> do
            meth <- rqMethod `fmap` getRequest
            when (meth == POST) $ handleSave file
            html <- liftIO $ mkSrcPage file
            htmlResponse html

handleSave :: FilePath -> Snap()
handleSave file = do
    contents <- getParam (C.pack "contents")
    case contents of
        Nothing -> errorBadRequest
        Just c -> liftIO $ B.writeFile file $ clean c
  where
    clean = T.encodeUtf8 . T.filter (/= '\x200b') . T.decodeUtf8
    

mkSrcPage :: FilePath -> IO Html
mkSrcPage path = do
    si <- getSrcInfo path
    contents <- if srcExists si
        then readFile (srcPath si)
        else return $ emptyModule (srcPath si)
    return $ srcPage si contents

srcPage :: SrcInfo -> String -> Html
srcPage si contents = devpage ("Source of " ++ srcPath si)
    [ h1 << srcPath si
    , p << small << srcFullPath si
    , form ! [Html.method "POST", identifier "editor"] <<
        [ input ! [thetype "button", value "Edit", identifier "btn-edit"],
          textarea ! [theclass "src", name "contents", identifier "txt-src",
              strAttr "readonly" "readonly" ] << contents
        , input ! [thetype "button", value "Cancel", identifier "btn-cancel",
              strAttr "disabled" "disabled"]
        , input ! [thetype "submit", value "Save", identifier "btn-save",
              strAttr "disabled" "disabled"]
        ]
    , preview si
    ]
    [ modFStat si, modActions si, modSearch]
    scriptSrcs

preview :: SrcInfo -> Html
preview = maybe noHtml build . previewPath
  where
    build p = 
        thediv ! [ theclass "with-preview" ] <<
            [ h1 << "Rendering Preview"
            , tag "iframe" ! [src p, identifier "preview"] << noHtml
            ]

modFStat :: SrcInfo -> Html
modFStat si = (h2 << "File Info") +++
    if srcExists si
        then [if srcWritable si then noHtml else p << bold << "read only",
              p << show (srcModTime si)]
        else [p << bold << "new file"]

modActions :: SrcInfo -> Html
modActions si = (h2 << "Actions") +++
    unordList (catMaybes
              [ previewLink si
              , Just $ italics << "Revert"
              , downloadLink si
              , fileLink si
              ])

modSearch :: Html
modSearch = (h2 << "Research") +++
    [ form ! [action "http://holumbus.fh-wedel.de/hayoo/hayoo.html"
                , target "barley-reseach"] <<
        [ input ! [thetype "text", name "query", theclass "research-query"]
        , input ! [thetype "submit", value "Hayoo"]
        ]
    , form ! [action "http://haskell.org/hoogle", target "barley-reseach"] <<
        [ input ! [thetype "text", name "q", theclass "research-query"] 
        , input ! [thetype "submit",  value "Hoogle"]
        ]
    ]
              
emptyModule :: FilePath -> String
emptyModule filename = 
    "module " ++ modName ++ " where\n\
    \\n\
    \import Text.Html\n\
    \\n\
    \page = body << [\n\
    \    h1 << \"Hi!\",\n\
    \    paragraph << \"testing\"\n\
    \    ]\n"
 where
   modName = filename  -- TODO should replace slashes with dots

scriptSrcs :: [String]
scriptSrcs =
    [ "/static/jquery.js"
    , "/static/codemirror_min.js"
    , "Source.js"
    ]

