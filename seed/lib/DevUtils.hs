module DevUtils (
    devpage,
    
    legalPath,
    
    SrcInfo(..), srcInfo,
    previewPath,
    
    finishWithError, errorBadRequest,
    ) where
    
import qualified Data.ByteString.Char8 as C
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

data SrcInfo = SrcInfo { siPath :: FilePath
                       , siFullPath :: FilePath
                       , siExists :: Bool
                       , siWritable :: Bool
                       , siModTime :: ClockTime
                       , siContents :: Maybe String
                       }


srcInfo :: FilePath -> IO SrcInfo
srcInfo path = do
    cwd <- getCurrentDirectory
    let fullPath = cwd </> path
    exists <- doesFileExist path
    canWrite <- if exists then writable `fmap` getPermissions path else return False
    modTime <- if exists then getModificationTime path else getClockTime
    contents <- if exists then Just `fmap` readFile fullPath else return Nothing
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
  
-- copied from Barley.Utils for now as there is no way to import it
  
-- | Immediately finish with an HTTP error status
finishWithError :: Int -> String -> Snap ()
finishWithError status message =
    finishWith $ setResponseStatus status (C.pack message) emptyResponse

-- | Common HTTP error statuses
errorBadRequest :: Snap ()
errorBadRequest = finishWithError 400 "Bad Request"


