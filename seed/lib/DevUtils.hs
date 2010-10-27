module DevUtils (
    devpage,
    
    legalPath,
    
    SrcInfo(..), getSrcInfo,
    SrcClass(..),
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
    ulist ! [theclass "left"] << li <<
         [ anchor ! [href "http://haskell.org/", theclass "logo"] << "Haskell" 
         , unordList $ map makelink communityLinks
         ]
    , ulist ! [theclass "right"] << map (li . makelink) siteLinks
    ]
  where
    makelink (title, url) = anchor ! [href url] << title
    communityLinks =
        [ ("Platform", "http://hackage.haskell.org/platform/")
        , ("Hackage", "http://hackage.haskell.org/packages/hackage.html")
        , ("Reddit", "http://www.reddit.com/r/haskell/")
        , ("Stack Overflow",
            "http://stackoverflow.com/questions/tagged?tagnames=haskell")
        ]
    siteLinks =
        [ ("Home", "/")
        , ("Project", "/project")
        , ("Documentation", "/doc")
        , ("Help", "/help")
        ]

scripts :: [String] -> Html
scripts = toHtml . map script
  where
    script s = tag "script" ! [ thetype "text/javascript", src s ] << noHtml


--
-- PROJECT FILE UTILITIES
--

data SrcInfo = SrcInfo { srcPath     :: FilePath -- | path relative to project
                       , srcFullPath :: FilePath -- | absolute path
                       , srcExists   :: Bool
                       , srcWritable :: Bool
                       , srcModTime  :: Maybe ClockTime
                       , srcClass    :: SrcClass
                       }

data SrcClass = SCPage | SCImage | SCScript | SCText | SCOther | SCDir
    deriving (Eq)
    
getSrcInfo :: FilePath -> IO SrcInfo
getSrcInfo path = do
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
            then SCDir
            else M.findWithDefault SCOther (takeExtension path) extToSrcClass
    return SrcInfo { srcPath = path
                   , srcFullPath = cwd </> path
                   , srcExists = exists
                   , srcWritable = canWrite
                   , srcModTime = modTime
                   , srcClass = cls
                   }

extToSrcClass = M.fromList
    [ (".html", SCPage)
    , (".xhtml",SCPage)
    , (".txt",  SCPage)
    , (".hs",   SCScript)
    , (".css",  SCText)
    , (".js",   SCText)
    , (".json", SCText)
    , (".xml",  SCText)
    , (".gif",  SCImage)
    , (".jpeg", SCImage)
    , (".jpg",  SCImage)
    , (".pdf",  SCImage)
    , (".png",  SCImage)
    , (".svg",  SCImage)
    ]
      
previewPath :: SrcInfo -> Maybe FilePath
previewPath si = if not (srcExists si) || libDir
                        then Nothing
                        else pp $ srcClass si
  where
    libDir = case splitDirectories path of
                        ("lib":_) -> True
                        _ -> False
    path = srcPath si
    pp SCPage = Just path
    pp SCScript = Just $ dropExtension path
    pp SCImage = Just path
    pp _ = Nothing

previewLink :: SrcInfo -> Maybe Html
previewLink si = build (srcClass si) `fmap` previewPath si
  where
    build fc p = anchor ! [href p, target "_blank", theclass "op-preview",
                    title ("View the " ++ long fc ++ "in another window")]
                    << thespan << ("View " ++ short fc)
                    
    short SCPage = "Page"
    short SCScript = "Page"
    short SCImage = "Image"
    short SCText = "Text"
    short SCOther = "File"
    short SCDir = "Dir"

    long SCPage = "page"
    long SCScript = "generated page"
    long SCImage = "image"
    long SCText = "text"
    long SCOther = "file"
    long SCDir = "dir"

editLink :: SrcInfo -> Maybe Html
editLink si = build `fmap` ee (srcClass si)
  where
    build n = anchor ! [href src, theclass "op-edit", title ("Edit the " ++ n)]
                << thespan << "Edit"
    src = "source?file=" ++ srcPath si
    ee SCPage = Just "page"
    ee SCScript = Just "script"
    ee SCText = Just "text"
    ee _ = Nothing
    
downloadLink :: SrcInfo -> Maybe Html
downloadLink si = build `fmap` dd (srcClass si)
  where
    build n = anchor ! [href (srcPath si), theclass "op-download",
                    title ("Download the " ++ n)]
                    << thespan << "Download"
    dd SCText = Just "text"
    dd SCScript = Just "script"
    dd _ = Nothing

fileLink :: SrcInfo -> Maybe Html
fileLink si = Just $ anchor ! [href ("file://" ++ srcFullPath si),
                    theclass "op-file",
                    title "Provides a file:// scheme URL to the local file"]
                    << thespan << "Local File"


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


