module Source where

import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Snap.Types
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath ((</>))
import Text.Html hiding ((</>))

handler :: Snap ()
handler = do
    mfile <- getParam (C.pack "file")
    case mfile of
        Just file -> liftIO (mkSrcPage (C.unpack file)) >>= returnHtml
        Nothing   -> returnHtml $ mkNewSrcPage "<rename me>"
  where
    returnHtml html = do 
        modifyResponse $ setContentType (C.pack "text/html; charset=UTF-8")
        writeBS $ (T.encodeUtf8 . T.pack) $ renderHtml html

mkSrcPage :: FilePath -> IO Html
mkSrcPage filename = do
    cwd <- getCurrentDirectory
    let fullname = cwd </> filename
    isFile <- doesFileExist fullname
    if isFile
        then readFile fullname >>= return . srcPage filename
        else return $ mkNewSrcPage filename

mkNewSrcPage :: FilePath -> Html
mkNewSrcPage filename = srcPage filename (emptyModule filename)

srcPage :: FilePath -> String -> Html
srcPage filename contents =
  thehtml << [
    header << [
      thelink ! [href "static/scaffold.css", rel "stylesheet",
                   thetype "text/css"] << noHtml,
      thetitle << ("Source of " ++ filename)
      ],
    body << [
      thediv ! [identifier "content", theclass "with-sidebar"] << [
        h1 << filename,
        pre ! [theclass "src"] << contents,
        sidebar
        ]
      ]
    ]

sidebar :: Html
sidebar = thediv ! [identifier "sidebar"] <<
    map (thediv ! [theclass "module"]) [ modFStat, modActions ]

modFStat :: Html
modFStat = (h2 << "File Info") +++
    (p << ("Sure be nice to have some fstat info here."))

modActions :: Html
modActions = (h2 << "Actions") +++
    unordList [ "View", "Edit", "Revert" ] +++
    unordList [ "Show", "Side & Side" ]

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

