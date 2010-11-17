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

module Barley.Utils (
    -- File Utilties
    legalPath,
    
    -- Snap Utilities
    finish,
    routeWhen, routeWhenIO,

    finishWithError,
    errorBadRequest, errorForbidden, errorNotFound, errorMethodNotAllowed,
    
    ) where

import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as C
import Snap.Types
import System.FilePath (joinPath, splitDirectories)

-- | Sanitize a file path for serving.
-- If the path contains any illegal path components, then Nothing is returned.
-- Otherwise a new FilePath, built from the dissected components is returned.
--
-- Illegal path components are any that start with a period, including "." and
-- "..", as well as those that contain any of '/', '\', or ':', or are empty.
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
-- These should be in Snap
--

-- | Immediately exit with what ever response was set in the Snap monad.
finish :: Snap a
finish = getResponse >>= finishWith    

-- | If the test is true, then finish immediately with the given action.
routeWhen :: Bool -> Snap () -> Snap ()
routeWhen f act = if f then act >> finish else return ()

-- | Like routeWhen, but test is provided in the IO monad.
routeWhenIO :: IO Bool -> Snap () -> Snap ()
routeWhenIO fio act = do
    f <- liftIO fio
    if f then act else return ()


-- | Immediately finish with an HTTP error status
finishWithError :: Int -> String -> Snap ()
finishWithError status message =
    finishWith $ setResponseStatus status (C.pack message) emptyResponse

-- | Common HTTP error statuses
errorBadRequest, errorForbidden, errorNotFound, errorMethodNotAllowed :: Snap ()
errorBadRequest = finishWithError 400 "Bad Request"
errorForbidden = finishWithError 403 "Forbidden"
errorNotFound = finishWithError 404 "Not Found"
errorMethodNotAllowed = finishWithError 405 "Method Not Allowed"
