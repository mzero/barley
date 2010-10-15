module Main (main) where

import Barley.Project
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Prelude hiding (init, mod)
import Snap.Http.Server
import Snap.Types
import System.Directory (doesDirectoryExist, doesFileExist,
            getCurrentDirectory)
import System.Environment
import System.Exit
import System.FilePath ((<.>), (</>), takeExtension)
import System.Plugins
import Text.Html hiding ((</>), address, content, start)

main :: IO ()
main = do
    args <- parseArgs
    case args of
        Just ("start", pd) -> start pd
        Just ("init", pd) -> init True pd
        Just ("run", pd) -> run pd
        Just (cmd, _) -> do putStrLn $ "unknown command: " ++ cmd
                            exitFailure
        Nothing -> do putStrLn "Usage: barley <command> [project dir]"
                      exitFailure

parseArgs :: IO (Maybe (String, ProjectDir))
parseArgs = do
    args <- getArgs
    case args of
        [] -> return Nothing
        [cmd] -> return $ Just (cmd, CurrentDir)
        [cmd,fp] -> return $ Just (cmd, ProjectDir fp)
        _ -> putStrLn "Too many arguments." >> return Nothing

-- | Create a project directory structure and run the web server.
start :: ProjectDir -> IO ()
start pd = init False pd >> run pd

-- | Run the web server.
run :: ProjectDir -> IO ()
run pd = do
    enter pd
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
    return $ prettyHtml html
        -- renderHtml would be more efficient, and perhaps more correct
        -- but it puts the content into an additional HTML element
        -- (for some ungodly reason)

serveTemplate :: FilePath -> Snap ()
serveTemplate filename = do
    html <- liftIO $ compile filename
    modifyResponse $ setContentType (C.pack "text/html; charset=UTF-8")
    writeBS $ (T.encodeUtf8 . T.pack) html

serveStatic :: FilePath -> Snap ()
serveStatic filename = do
    modifyResponse $ setContentType (C.pack mimeType)
    sendFile filename
  where
    mimeType = M.findWithDefault defMimeType extension extToMimeType
    extension = takeExtension filename
    defMimeType = "application/octet-stream" 
    extToMimeType = M.fromList
        [ (".css",  "text/css")
        , (".gif",  "image/gif")
        , (".html", "text/thml")
        , (".jpeg", "image/jpeg")
        , (".jpg",  "image/jpeg")
        , (".js",   "application/javascript")
        , (".json", "application/json")
        , (".pdf",  "application/pdf")
        , (".png",  "image/png")
        , (".svg",  "image/svg+xml")
        , (".txt",  "text/plain")
        , (".xhtml","application/xhtml+xml")
        , (".xml",  "application/xml")
        ]
        
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
