module Main (main) where

import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as C
import Prelude hiding (init)
import Snap.Http.Server
import Snap.Types
import System.Directory (getCurrentDirectory)
import System.Environment
import System.Exit
import System.FilePath ((</>))
import System.Plugins
import Text.Html hiding ((</>), start)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["start"] -> start
        ["init"] -> init
        ["run"] -> run
        [cmd] -> do putStrLn $ "unknown command: " ++ cmd
                    exitFailure
        _ -> putStrLn "Usage: barley <command>" >> exitFailure

-- | Create a project directory structure and run the web server.
start :: IO ()
start = init >> run

-- | Create a project directory structure.
init :: IO ()
init = return ()

-- | Run the web server.
run :: IO ()
run = do
    let address = "*"
        port = 8080
        hostname = "myserver"
    putStrLn $ "Running on http://localhost:" ++ show port ++ "/"
    httpServe (C.pack address) port (C.pack hostname) Nothing Nothing
        genericHandler

-- | Compile a template and return the generate HTML as a String.
compile :: FilePath -> IO String
compile filename = do
    status <- make filename []
    html <- case status of
        MakeSuccess _ objfile -> do
            putStrLn objfile
            loadStatus <- load_ objfile [] "page"
            case loadStatus of
                LoadSuccess _ page -> do
                    return $ (page :: Html)
                LoadFailure errs -> do
                    errorHtml errs filename
        MakeFailure errs -> do
            errorHtml errs filename
    return $ renderHtml html

-- | Given a URL, render the corresponding template.
genericHandler :: Snap ()
genericHandler = do
    -- TODO: Check if the URL points inside the templates directory.
    -- TODO: Server public/index.html for /
    uri <- rqURI `fmap` getRequest
    cwd <- liftIO getCurrentDirectory
    -- XXX: directory traversal
    html <- liftIO $ compile (cwd </> tail (C.unpack uri))
    modifyResponse $ setContentType (C.pack "text/html")
    writeBS (C.pack html)

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
