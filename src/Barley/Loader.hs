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

module Barley.Loader (
    EntryPoint(),
    entryPoint,
    
    compileAndLoadFirst,
    )
    where

import Barley.AltLoad -- instead of System.Plugins.Load
import Control.Monad (when)
import Data.IORef
import qualified Data.Map as M
import System.Directory (createDirectoryIfMissing)
import System.IO.Unsafe (unsafePerformIO)
import System.Plugins.Make
import System.Plugins.Utils (newer)



-- | An representation of a possible entry point in a file that, when
-- loaded, run, returns an a value of type a
newtype EntryPoint a = EntryPoint { loadEntryPoint :: Module -> IO (Maybe a) }

-- | Builds an entry point. The entry point is named with the given symbol
-- and returns a value of type a. The supplied transformer function is used
-- to supply a value of some type b. This is so that all entry points can
-- return the same type.
entryPoint :: String -> (a -> b) -> EntryPoint b
entryPoint symbol xform = EntryPoint (loadWith symbol xform)


-- | A mapping between source files and the module objects that are loaded
-- While System.Plugins should keep this information, it doesn't, so we have to.
type ModuleMap = M.Map FilePath Module

theModMap :: IORef ModuleMap
theModMap = unsafePerformIO $ newIORef M.empty
{-# NOINLINE theModMap #-}


-- | Given a file, try to make sure it is built and up-to-date
-- Can't use recompileAll here because it "hunts" for the source files (rather
-- than having Module remember them), and we store the objects in a different
-- directory than the sources. Hence, our test doesn't recursively look at
-- dependent modules. Fie!
compileModule :: FilePath -> IO (Either String Module)
compileModule srcFile = do
    mm <- readIORef theModMap
    let existingModule = M.lookup srcFile mm
    
    needBuild <- case existingModule of
        Just m -> srcFile `newer` path m
        Nothing -> return True        
    makeStatus <- case (needBuild, existingModule) of
        (False, Just m) -> return (MakeSuccess NotReq $ path m)
        _ -> noteMakeAll >> makeAll srcFile makeArgs

    case (makeStatus, existingModule) of
        (MakeSuccess NotReq _, _     ) -> return ()
        (_                   , Just m) -> noteUnloadAll >> unloadAll m
        (_                   , _     ) -> return ()
    
    case (makeStatus, existingModule) of
        (MakeFailure errs, _) -> do
            noteFailure errs
            writeIORef theModMap (M.delete srcFile mm)
            return $ Left $ unlines errs
        (MakeSuccess NotReq _, Just m) -> do
            noteNothing
            return (Right m)
        (MakeSuccess _ objFile, _) -> do
            noteLoading
            m <- loadObjectFile objFile [buildDir] []
            writeIORef theModMap (M.insert srcFile m mm)
            return (Right m)
  where
    buildDir = ".build"
    makeArgs =      
        [ "-ilib" -- users can put non-served source here 
        , "-outputdir", buildDir -- sets odir, hidir, and stubdir in one go
        , "-odir", buildDir -- plugins only looks for odir, not outputdir
        ]
        
    debug = False
    noteMakeAll      = when debug $ putStrLn ("makeAll of " ++ srcFile)
    noteUnloadAll    = when debug $ putStrLn "unloading..."
    noteFailure errs = when debug $ putStrLn ("failed: " ++ concat errs)
    noteNothing      = when debug $ putStrLn "nothin' to do"
    noteLoading      = when debug $ putStrLn "loading..."
    

-- | Given a file, compile and load the file, then load the value from the
-- first entry point that succeeds (Right). If either the compilation fails,
-- or the the loading fails, then an error string is returned (Left).
compileAndLoadFirst :: FilePath -> [EntryPoint a] -> IO (Either String a)
compileAndLoadFirst srcFile eps = do
    createDirectoryIfMissing False ".build" -- plugins needs it to pre-exist
    cs <- compileModule srcFile
    case cs of
        Right m -> tryEntryPoints m eps
        Left errs -> return $ Left errs

tryEntryPoints :: Module -> [EntryPoint a] -> IO (Either String a)
tryEntryPoints m (ep:eps) =
    loadEntryPoint ep m >>= maybe (tryEntryPoints m eps) (return . Right)
tryEntryPoints _ [] = return (Left "No matching entry points found.")

loadWith :: String -> (a -> b) -> Module -> IO (Maybe b)
loadWith sym xfn m = (xfn `fmap`) `fmap` loadFunction m sym
