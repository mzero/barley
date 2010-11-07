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
    showPreview <- (maybe False (not . C.null))
                    `fmap` getParam (C.pack "preview")
    case maybe Nothing (legalPath . C.unpack) fileparam of
        Nothing -> errorBadRequest
        Just file -> do
            meth <- rqMethod `fmap` getRequest
            when (meth == POST) $ handleSave file
            html <- liftIO $ mkSrcPage file showPreview
            htmlResponse html

handleSave :: FilePath -> Snap()
handleSave file = do
    contents <- getParam (C.pack "contents")
    case contents of
        Nothing -> errorBadRequest
        Just c -> liftIO $ B.writeFile file $ clean c
  where
    clean = T.encodeUtf8 . T.filter (/= '\x200b') . T.decodeUtf8
    

mkSrcPage :: FilePath -> Bool -> IO Html
mkSrcPage path showPreview = do
    si <- getSrcInfo path
    contents <- if srcExists si
        then readFile (srcPath si)
        else return $ emptyModule (srcPath si)
    return $ srcPage si contents showPreview

srcPage :: SrcInfo -> String -> Bool -> Html
srcPage si contents showPreview = devpage ("Source of " ++ srcPath si)
    [ h1 << srcPath si
    , p << small << srcFullPath si
    , if showPreview then preview si else noHtml
    , form ! [Html.method "POST", identifier "edit-form"] <<
        (btns ++ [editor] ++ btns ++ [hidden])
        ]
    [ modFStat si, modActions si, modSearch]
    scriptSrcs
  where
    btns = [ input ! [thetype "button", value "Cancel", theclass "btn-cancel"]
           , input ! [thetype "submit", value "Save", theclass "btn-save"]
           ]
    editor = thediv ! [identifier "editor"] <<
                textarea ! [theclass "src", name "contents",
                    identifier "txt-src", strAttr "readonly" "readonly" ]
                << contents
    hidden = input ! [thetype "hidden", name "preview", value "1"]

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
    ulist << map (li ! [theclass "op"]) (catMaybes
              [ previewLink si
              --, Just $ italics << "Revert"
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

