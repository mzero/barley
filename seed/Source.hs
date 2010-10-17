module Source where

import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Snap.Types
import System.Directory
import System.FilePath ((</>), dropExtension)
import System.Time (ClockTime, getClockTime)
import Text.Html hiding ((</>))

nu = () -- DO NOT DELETE THIS

handler :: Snap ()
handler = do
    file <- getParam (C.pack "file")
    html <- liftIO . mkSrcPage $ maybe "<rename me>" C.unpack file
    modifyResponse $ setContentType (C.pack "text/html; charset=UTF-8")
    writeBS $ (T.encodeUtf8 . T.pack) $ renderHtml html


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
        sidebar si
        ]
      ]
    ]


sidebar :: SrcInfo -> Html
sidebar si = thediv ! [identifier "sidebar"] <<
    map (thediv ! [theclass "module"]) [ modFStat si, modActions si]

modFStat :: SrcInfo -> Html
modFStat si = (h2 << "File Info") +++
    if siExists si
        then [if siWritable si then noHtml else p << bold << "read only",
              p << show (siModTime si)]
        else [p << bold << "new file"]

modActions :: SrcInfo -> Html
modActions si = (h2 << "Actions") +++
    unordList [ anchor ! [href (dropExtension $ siPath si)] << "View"
              , italics << "Edit"
              , italics << "Revert"
              ] +++
    unordList [ anchor ! [href ("file://" ++ siFullPath si)] << "File"
              , anchor ! [href (siPath si)] << "Download"
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

