module Source where

import DevUtils

import Control.Monad (when)
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Snap.Types
import qualified Snap.Types as Snap
import System.Directory
import System.FilePath ((</>), dropExtension, joinPath,
    splitDirectories, takeExtension)
import System.Time (ClockTime, getClockTime)
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
            modifyResponse $ setContentType (C.pack "text/html; charset=UTF-8")
            writeBS $ (T.encodeUtf8 . T.pack) $ renderHtml html

handleSave :: FilePath -> Snap()
handleSave file = do
    contents <- getParam (C.pack "contents")
    case contents of
        Nothing -> errorBadRequest
        Just c -> liftIO $ C.writeFile file c
    
data SrcInfo = SrcInfo { siPath :: FilePath
                       , siFullPath :: FilePath
                       , siExists :: Bool
                       , siWritable :: Bool
                       , siModTime :: ClockTime
                       , siContents :: String
                       }

srcInfo :: FilePath -> IO SrcInfo
srcInfo path = do
    cwd <- getCurrentDirectory
    let fullPath = cwd </> path
    exists <- doesFileExist path
    canWrite <- if exists then writable `fmap` getPermissions path else return False
    modTime <- if exists then getModificationTime path else getClockTime
    contents <- if exists then readFile fullPath else return (emptyModule path)
        -- maybe these should all be Maybe
    return SrcInfo { siPath = path
                   , siFullPath = fullPath
                   , siExists = exists
                   , siWritable = canWrite
                   , siModTime = modTime
                   , siContents = contents
                   }

previewPath :: SrcInfo -> Maybe FilePath
previewPath si = pp (siExists si) (takeExtension f) (splitDirectories f)
  where
    f = siPath si
    pp False _ _ = Nothing
    pp _ ".html" _ = Just f
    pp _ ".hs" ("lib":_) = Nothing
    pp _ ".hs" _ = Just $ dropExtension f
    pp _ _ _ = Nothing

mkSrcPage :: FilePath -> IO Html
mkSrcPage path = srcInfo path >>= return . srcPage


srcPage :: SrcInfo -> Html
srcPage si = devpage ("Source of " ++ siPath si)
    [ h1 << siPath si
    , p << small << siFullPath si
    , form ! [Html.method "POST", identifier "editor"] <<
        [ input ! [thetype "button", value "Edit", identifier "btn-edit"],
          textarea ! [theclass "src", name "contents", identifier "txt-src",
              strAttr "readonly" "readonly" ] << siContents si
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

--
-- These are copied from Barley.Utils for now as there is no way to import it
--
legalPath :: FilePath -> Maybe FilePath
legalPath p =
    if any illegalComponent components
        then Nothing
        else Just $ joinPath components
  where
    components = splitDirectories p
    illegalComponent "" = True
    illegalComponent ('.':_) = True
    illegalComponent s = any (`elem` "/\\:") s 
    
-- | Immediately finish with an HTTP error status
finishWithError :: Int -> String -> Snap ()
finishWithError status message =
    finishWith $ setResponseStatus status (C.pack message) emptyResponse

-- | Common HTTP error statuses
errorBadRequest :: Snap ()
errorBadRequest = finishWithError 400 "Bad Request"

