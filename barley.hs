module Main (main) where

import Control.Monad (when)
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as C
import Prelude hiding (init)
import Snap.Http.Server
import Snap.Types
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory,
                         getDirectoryContents)
import System.Environment
import System.Exit
import System.FilePath ((<.>), (</>))
import System.Plugins
import Text.Html hiding ((</>), address, content, start)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["start"] -> start
        ["init"] -> init True
        ["run"] -> run
        [cmd] -> do putStrLn $ "unknown command: " ++ cmd
                    exitFailure
        _ -> putStrLn "Usage: barley <command>" >> exitFailure

-- | Create a project directory structure and run the web server.
start :: IO ()
start = init False >> run

-- | Create a project directory structure.
init :: Bool -> IO ()
init warnIfNotEmpty = nothingHere >>= \b -> if b
    then copyInitialProject
    else when warnIfNotEmpty $
        putStrLn "This directory is not empty. Not initializing"
  where
    nothingHere = whatsHere >>= return . null . filter notDot
    whatsHere = getCurrentDirectory >>= getDirectoryContents 
    notDot ('.':_) = False
    notDot _ = True
    copyInitialProject = putStrLn "Should be creating the default project here"

-- | Run the web server.
run :: IO ()
run = do
    let address = "*"
        port = 8080
        hostname = "myserver"
    putStrLn $ "Running on http://localhost:" ++ show port ++ "/"
    httpServe (C.pack address) port (C.pack hostname) (Just "access.log")
        (Just "error.log") genericHandler

-- | Compile a template and return the generate HTML as a String.
compile :: FilePath -> IO String
compile filename = do
    status <- make filename []
    html <- case status of
        MakeSuccess _ objfile -> do
            loadStatus <- load_ objfile [] "page"
            case loadStatus of
                LoadSuccess mod page -> do page `seq` unloadAll mod
                                           return $ (page :: Html)
                LoadFailure errs -> errorHtml errs filename
        MakeFailure errs -> errorHtml errs filename
    return $ renderHtml html

serveTemplate :: FilePath -> Snap ()
serveTemplate filename = do
    html <- liftIO $ compile filename
    modifyResponse $ setContentType (C.pack "text/html")
    writeBS $ C.pack html

serveStatic :: FilePath -> Snap ()
serveStatic filename = do
    modifyResponse $ setContentType (C.pack "text/html")
    sendFile filename

-- | Given a URL, render the corresponding template.
genericHandler :: Snap ()
genericHandler = do
    uri <- rqURI `fmap` getRequest
    cwd <- liftIO getCurrentDirectory

    -- XXX: directory traversal
    let filename = cwd </> tail (C.unpack uri)
    isFile <- liftIO $ doesFileExist filename
    if isFile
        then serveStatic filename
        else do
            isDir <- liftIO $ doesDirectoryExist filename
            if isDir
                then do
                    let tmpl = filename </> "index.hs"
                    serveTemplateIfExists tmpl
                else serveTemplateIfExists $ filename <.> "hs"
  where
    serveTemplateIfExists :: FilePath -> Snap ()
    serveTemplateIfExists tmpl = do
        isFile <- liftIO $ doesFileExist tmpl
        if isFile
            then serveTemplate tmpl
            else pass

-- | Given a list of errors and a template, create an HTML page that
-- displays the errors.
errorHtml :: Errors -> FilePath -> IO Html
errorHtml errs filename = do
    content <- readFile filename
    length content `seq` return ()
    let html = thehtml <<
               body << [
                   thediv ! [theclass "errors"] << [
                        h2 << "Errors",
                        pre << unlines errs
                        ],
                   pre ! [theclass "sourcefile"] << content
               ]
    return html
