module Source where

import Control.Monad (when)
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Snap.Types
import qualified Snap.Types as Snap
import System.Directory
import System.FilePath ((</>), dropExtension)
import System.Time (ClockTime, getClockTime)
import Text.Html hiding ((</>))
import qualified Text.Html as Html

nu = () -- DO NOT DELETE THIS

handler :: Snap ()
handler = do
    meth <- rqMethod `fmap` getRequest
    when (meth == POST) handleSave
    file <- getParam (C.pack "file")
    html <- liftIO . mkSrcPage $ maybe "<rename me>" C.unpack file
    modifyResponse $ setContentType (C.pack "text/html; charset=UTF-8")
    writeBS $ (T.encodeUtf8 . T.pack) $ renderHtml html

handleSave :: Snap()
handleSave = do
    file <- getParam (C.pack "file")
    contents <- getParam (C.pack "contents")
    save file contents
  where
    save (Just f) (Just c) = liftIO $ writeFile (C.unpack f) (C.unpack c)
    save _ _ = modifyResponse $ setResponseCode 400
    
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

mkSrcPage :: FilePath -> IO Html
mkSrcPage path = srcInfo path >>= return . srcPage


srcPage :: SrcInfo -> Html
srcPage si =
  thehtml << [
    header << [
      thelink ! [href "static/scaffold.css", rel "stylesheet",
                   thetype "text/css"] << noHtml,
      thetitle << ("Source of " ++ siPath si)
      ],
    body << [
      thediv ! [identifier "content", theclass "with-sidebar"] << [
        h1 << siPath si,
        p << small << siFullPath si,
        pre ! [theclass "src"] << siContents si,
        form ! [Html.method "POST", identifier "editor"] <<
          [ textarea ! [theclass "src", name "contents"] << siContents si
          , input ! [thetype "submit", value "Save"]
          ],
        sidebar si
        ]
      ]
    ]


sidebar :: SrcInfo -> Html
sidebar si = thediv ! [identifier "sidebar"] <<
    map (thediv ! [theclass "module"]) [ modFStat si, modActions si, modSearch]

modFStat :: SrcInfo -> Html
modFStat si = (h2 << "File Info") +++
    if siExists si
        then [if siWritable si then noHtml else p << bold << "read only",
              p << show (siModTime si)]
        else [p << bold << "new file"]

modActions :: SrcInfo -> Html
modActions si = (h2 << "Actions") +++
    unordList [ anchor ! [href (dropExtension $ siPath si), target "barley-run",
                    title "Run this code by browsing its page in another window"]
                    << "Run"
              , italics << "Edit"
              , italics << "Revert"
              ] +++
    unordList [ anchor ! [href ("file://" ++ siFullPath si),
                    title "Provides a file:// scheme URL to the local file"]
                    << "Local File"
              , anchor ! [href (siPath si)] << "Download"
              ]

modSearch :: Html
modSearch = (h2 << "Research") +++
    [ form ! [action "http://holumbus.fh-wedel.de/hayoo/hayoo.html"
                , target "barley-reseach"] <<
        [ input ! [thetype "text", name "query"]
        , input ! [thetype "submit", value "Hayoo"]
        ]
    , form ! [action "http://haskell.org/hoogle", target "barley-reseach"] <<
        [ input ! [thetype "text", name "q"] 
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

