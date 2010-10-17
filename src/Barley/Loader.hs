module Barley.Loader (
    EntryPoint(),
    entryPoint,
    
    compileAndLoadFirst,
    )
    where

import System.Plugins


-- | An representation of a possible entry point in a file that, when
-- loaded, run, returns an a value of type a
newtype EntryPoint a = EntryPoint { loadEntryPoint :: Module -> IO (Maybe a) }

-- | Builds an entry point. The entry point is named with the given symbol
-- and returns a value of type a. The supplied transformer function is used
-- to supply a value of some type b. This is so that all entry points can
-- return the same type.
entryPoint :: String -> (a -> b) -> EntryPoint b
entryPoint symbol xform = EntryPoint (loadWith symbol xform)

-- | Given a file, compile and load the file, then load the value from the
-- first entry point that succeeds (Right). If either the compilation fails,
-- or the the loading fails, then an error string is returned (Left).
compileAndLoadFirst :: FilePath -> [EntryPoint a] -> IO (Either String a)
compileAndLoadFirst srcFile eps = do
    status <- make srcFile []
    case status of
        MakeSuccess _ objFile -> loadFirst objFile eps
        MakeFailure errs -> return $ Left $ unlines errs

-- This code would like to use the low level interface in System.Plugins.Load,
-- so that the module can be loaded once, and then each entry point probed for.
-- BUT, the low level interface doesn't actually expose enough machinery to do
-- so. See, for example, how the implementation of load uses several functions
-- that are not exported and are rather too complicated to replicate.

-- The current hack is to insist that every loaded module have a symbol nu.
-- This code uses the high level interface to load that module, which succeeds
-- and then uses the low level interface to probe the other entry points.

loadFirst :: FilePath -> [EntryPoint a] -> IO (Either String a)
loadFirst objFile eps = do
    v <- load_ objFile [] "nu"
    case v of
        LoadFailure _ -> return (Left "yer dead now: no nu")
        LoadSuccess m _ -> do
            w <- tryEntryPoints m eps
            w `seq` unloadAll m
            return w

tryEntryPoints :: Module -> [EntryPoint a] -> IO (Either String a)
tryEntryPoints m (ep:eps) =
    loadEntryPoint ep m >>= maybe (tryEntryPoints m eps) (return . Right)
tryEntryPoints _ [] = return (Left "No matching entry points found.")

loadWith :: String -> (a -> b) -> Module -> IO (Maybe b)
loadWith sym xfn m = (xfn `fmap`) `fmap` loadFunction m sym
