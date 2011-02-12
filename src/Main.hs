-- Copyright 2010 Google Inc.
-- 
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
-- 
--      http://www.apache.org/licenses/LICENSE-2.0
-- 
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module Main (main) where

import Barley.Loader
import Barley.Project
import Barley.Utils
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M
import Data.Maybe
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
import System.PosixCompat.Files (getFileStatus, modificationTime)
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
        config = foldr ($) emptyConfig
            [ addListen (ListenHttp (C.pack address) port)
            , setAccessLog (Just "access.log")
            , setErrorLog (Just "error.log")
            ]
    putStrLn $ "Running on http://localhost:" ++ show port ++ "/"
    httpServe config genericHandler


type CompiledTemplate = Either String (Snap ())
-- | Compile a template to either an error string, or a Snap action
compileTemplate :: FilePath -> IO CompiledTemplate
compileTemplate filename = compileAndLoadFirst filename templateEntryPoints
  where
    templateEntryPoints =
        [ entryPoint "handler" id
        , entryPoint "page" htmlResult
        , entryPoint "output" stringResult
        ]
  
-- | Run a compiled template as a Snap action.
runTemplate :: FilePath -> CompiledTemplate -> Snap ()
runTemplate filename compilation = do
    either errorResult id $ compilation
  where
    errorResult errs = liftIO (errorHtml errs filename) >>= htmlResult

htmlResult :: Html -> Snap ()
htmlResult html = do
    modifyResponse $ setContentType (C.pack "text/html; charset=UTF-8")
    writeBS $ (T.encodeUtf8 . T.pack) $ renderHtml html
        -- warning: renderHTML wraps an additional HTML element around the
        -- content (for some ungodly reason)

stringResult :: String -> Snap ()
stringResult s = do
    modifyResponse $ setContentType (C.pack "text/plain; charset=UTF-8")
    writeBS $ (T.encodeUtf8 . T.pack) s

serveTemplate :: FilePath -> Snap ()
serveTemplate filename = do
    compilation <- liftIO $ compileTemplate filename
    compileOnly <- getParam (C.pack "__compile_only")
    case C.unpack `fmap` compileOnly of
        Just c | c == "1"  -> stringResult $ either id (const "OK") compilation
               | otherwise -> errorBadRequest
        Nothing            -> runTemplate filename compilation

data TypeInfo = TI { tiMimeType :: String, tiMaxAge :: Maybe Int }

extToTypeInfo :: M.Map String TypeInfo
extToTypeInfo = M.fromList
    [ (".css",  srcType "text/css")
    , (".gif",  imgType "image/gif")
    , (".html", srcType "text/html")
    , (".jpeg", imgType "image/jpeg")
    , (".jpg",  imgType "image/jpeg")
    , (".js",   srcType "application/javascript")
    , (".json", srcType "application/json")
    , (".pdf",  imgType "application/pdf")
    , (".png",  imgType "image/png")
    , (".svg",  imgType "image/svg+xml")
    , (".txt",  srcType "text/plain")
    , (".xhtml",srcType "application/xhtml+xml")
    , (".xml",  srcType "application/xml")
    ]
  where
    srcType s = TI s (Just 30)   -- revalidate things we might edit every 30 seconds
    imgType s = TI s (Just 3600) -- images are stable for at least an hour

defTypeInfo :: TypeInfo
defTypeInfo = TI "application/octet-stream" Nothing

serveStatic :: FilePath -> Snap ()
serveStatic filename = do
    fstat <- liftIO $ getFileStatus filename
    processModificationTime (modificationTime fstat) (tiMaxAge typeInfo)
    modifyResponse $ setContentType (C.pack $ tiMimeType typeInfo)
    sendFile filename
  where
    typeInfo = M.findWithDefault defTypeInfo extension extToTypeInfo
    extension = takeExtension filename

        
-- | Given a URL, render the corresponding template.
genericHandler :: Snap ()
genericHandler = do
    mpath <- (legalPath . C.unpack . rqPathInfo) `fmap` getRequest
    case mpath of
        Nothing -> errorNotFound
        Just relpath -> do
            cwd <- liftIO getCurrentDirectory
            let fullpath = cwd </> relpath
            routeWhenIO (doesFileExist fullpath)      $ serveStatic fullpath
            routeWhenIO (doesDirectoryExist fullpath) $ serveIndex fullpath
            serveTemplateIfExists $ fullpath <.> "hs"
  where
    serveIndex :: FilePath -> Snap ()
    serveIndex fullpath = serveTemplateIfExists $ fullpath </> "Index.hs"
    serveTemplateIfExists :: FilePath -> Snap ()
    serveTemplateIfExists tmplpath = do
        routeWhenIO (doesFileExist tmplpath) $ serveTemplate tmplpath

-- | Given a list of errors and a template, create an HTML page that
-- displays the errors.
errorHtml :: String -> FilePath -> IO Html
errorHtml errs filename = do
    content <- readFile filename
    length content `seq` return ()
    let html = thehtml <<
               body << [
                   thediv ! [theclass "errors"] << [
                        h2 << "Errors",
                        pre << errs
                        ],
                   pre ! [theclass "sourcefile"] << content
               ]
    return html

