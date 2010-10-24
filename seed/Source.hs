module Source where

import DevUtils

import Control.Monad (when)
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as C
import Data.Maybe
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
        Just c -> liftIO $ C.writeFile file c
    

mkSrcPage :: FilePath -> IO Html
mkSrcPage path = srcInfo path >>= return . srcPage


srcPage :: SrcInfo -> Html
srcPage si = devpage ("Source of " ++ siPath si)
    [ h1 << siPath si
    , p << small << siFullPath si
    , form ! [Html.method "POST", identifier "editor"] <<
        [ input ! [thetype "button", value "Edit", identifier "btn-edit"],
          textarea ! [theclass "src", name "contents", identifier "txt-src",
              strAttr "readonly" "readonly" ] <<
                    fromMaybe (emptyModule $ siPath si) (siContents si)
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
    if siExists si
        then [if siWritable si then noHtml else p << bold << "read only",
              p << show (siModTime si)]
        else [p << bold << "new file"]

modActions :: SrcInfo -> Html
modActions si = (h2 << "Actions") +++
    unordList (catMaybes
              [ (\p ->
                 anchor ! [href p, target "_blank",
                    title "View the generated page in another window"]
                    << "View Page"
                 ) `fmap` previewPath si
              , Just $ italics << "Revert"
              ]) +++
    unordList [ anchor ! [href ("file://" ++ siFullPath si),
                    title "Provides a file:// scheme URL to the local file"]
                    << "Local File"
              , anchor ! [href (siPath si)] << "Download"
              ]

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
    [ "static/jquery.js"
    , "static/jquery.elastic.js"
    , "Source.js"
    ]

