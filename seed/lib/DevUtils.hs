module DevUtils (
    devpage,
    
    legalPath,
    
    SrcInfo(..), srcInfo,
    FileClass(..),
    previewPath,
    previewLink, editLink, downloadLink, fileLink,
    
    htmlResponse,
    finishWithError, errorBadRequest,
    ) where
    
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Snap.Types
import System.Directory
import System.FilePath ((</>), dropExtension, joinPath,
    splitDirectories, takeExtension)
import System.Time (ClockTime, getClockTime)
import Text.Html hiding ((</>))

--
-- DEV PAGE LAYOUT
--

devpage :: HTML a => String -> a -> [Html] -> [String] -> Html
devpage pageTitle contents modules scriptSrcs =
    header << [
      thelink ! [href "static/scaffold.css", rel "stylesheet",
                   thetype "text/css"] << noHtml,
      thetitle << fullTitle
      ] +++
    body ! [theclass "with-topbar"] << [
      h1 ! [identifier "logo"] << "Barley",
      thediv ! [identifier "content", theclass "with-sidebar"] <<
        (toHtml contents +++ toHtml (sidebar modules)),
      topbar,
      scripts scriptSrcs
      ]
  where
    fullTitle = if null pageTitle then "Barley" else "Barley - " ++ pageTitle

sidebar :: [Html] -> Html
sidebar modules = thediv ! [identifier "sidebar"] <<
    map (thediv ! [theclass "module"]) modules

topbar :: Html
topbar = thediv ! [identifier "topbar"] << [
    p << makelink haskellLink,
    unordList $ map makelink communityLinks
    ]
  where
    makelink (title, url) = anchor ! [href url] << title
    haskellLink = ("Haskell", "http://haskell.org/")
    communityLinks =
        [ ("Platform", "http://hackage.haskell.org/platform/")
        , ("Hackage", "http://hackage.haskell.org/packages/hackage.html")
        , ("λ Reddit", "http://www.reddit.com/r/haskell/")
        , ("λ Stack Overflow",
            "http://stackoverflow.com/questions/tagged?tagnames=haskell")
        ]

scripts :: [String] -> Html
scripts = toHtml . map script
  where
    script s = tag "script" ! [ thetype "text/javascript", src s ] << noHtml


--
-- PROJECT FILE UTILITIES
--

data SrcInfo = SrcInfo { siPath     :: FilePath -- | path relative to project
                       , siFullPath :: FilePath -- | absolute path
                       , siExists   :: Bool
                       , siWritable :: Bool
                       , siModTime  :: Maybe ClockTime
                       , siClass    :: FileClass
                       }

data FileClass = FCPage | FCImage | FCScript | FCText | FCOther | FCDir
    deriving (Eq)
    
srcInfo :: FilePath -> IO SrcInfo
srcInfo path = do
    cwd <- getCurrentDirectory
    isDir <- doesDirectoryExist path
    isFile <- doesFileExist path
    let exists = isDir || isFile
    canWrite <- if exists
        then writable `fmap` getPermissions path 
        else return False -- TODO: should be writable of containing dir
    modTime <- if exists
        then Just `fmap` getModificationTime path
        else return Nothing
    let cls = if isDir
            then FCDir
            else M.findWithDefault FCOther (takeExtension path) extToFileClass
    return SrcInfo { siPath = path
                   , siFullPath = cwd </> path
                   , siExists = exists
                   , siWritable = canWrite
                   , siModTime = modTime
                   , siClass = cls
                   }

extToFileClass = M.fromList
    [ (".html", FCPage)
    , (".xhtml",FCPage)
    , (".txt",  FCPage)
    , (".hs",   FCScript)
    , (".css",  FCText)
    , (".js",   FCText)
    , (".json", FCText)
    , (".xml",  FCText)
    , (".gif",  FCImage)
    , (".jpeg", FCImage)
    , (".jpg",  FCImage)
    , (".pdf",  FCImage)
    , (".png",  FCImage)
    , (".svg",  FCImage)
    ]
      
previewPath :: SrcInfo -> Maybe FilePath
previewPath si = if not (siExists si) || libDir
                        then Nothing
                        else pp $ siClass si
  where
    libDir = case splitDirectories path of
                        ("lib":_) -> True
                        _ -> False
    path = siPath si
    pp FCPage = Just path
    pp FCScript = Just $ dropExtension path
    pp FCImage = Just path
    pp _ = Nothing

previewLink :: SrcInfo -> Maybe Html
previewLink si = build (siClass si) `fmap` previewPath si
  where
    build fc p = anchor ! [href p, target "_blank",
                    title ("View the " ++ long fc ++ "in another window")]
                    << ("View " ++ short fc)
                    
    short FCPage = "Page"
    short FCScript = "Page"
    short FCImage = "Image"
    short FCText = "Text"
    short FCOther = "File"
    short FCDir = "Dir"

    long FCPage = "page"
    long FCScript = "generated page"
    long FCImage = "image"
    long FCText = "text"
    long FCOther = "file"
    long FCDir = "dir"

editLink :: SrcInfo -> Maybe Html
editLink si = build `fmap` ee (siClass si)
  where
    build n = anchor ! [href src, title ("Edit the " ++ n)] << "Edit"
    src = "source?file=" ++ siPath si
    ee FCPage = Just "page"
    ee FCScript = Just "script"
    ee FCText = Just "text"
    ee _ = Nothing
    
downloadLink :: SrcInfo -> Maybe Html
downloadLink si = build `fmap` dd (siClass si)
  where
    build n = anchor ! [href (siPath si), title ("Download the " ++ n)]
                    << "Download"
    dd FCText = Just "text"
    dd FCScript = Just "script"
    dd _ = Nothing

fileLink :: SrcInfo -> Maybe Html
fileLink si = Just $ anchor ! [href ("file://" ++ siFullPath si),
                    title "Provides a file:// scheme URL to the local file"]
                    << "Local File"


-- copied from Barley.Utils for now as there is no way to import it
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


--
-- SNAP UTILITIES
--

htmlResponse :: HTML a => a -> Snap ()
htmlResponse html = do
    modifyResponse $ setContentType (C.pack "text/html; charset=UTF-8")
    writeBS $ (T.encodeUtf8 . T.pack) $ renderHtml html

-- copied from Barley.Utils for now as there is no way to import it
  
-- | Immediately finish with an HTTP error status
finishWithError :: Int -> String -> Snap ()
finishWithError status message =
    finishWith $ setResponseStatus status (C.pack message) emptyResponse

-- | Common HTTP error statuses
errorBadRequest :: Snap ()
errorBadRequest = finishWithError 400 "Bad Request"


