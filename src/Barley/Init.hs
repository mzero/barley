module Barley.Init (
    init
    ) where

import Control.Monad (when)
import Prelude hiding (init)
import System.Directory (getCurrentDirectory, getDirectoryContents)

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
    
