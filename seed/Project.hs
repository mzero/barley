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

module Project where

import DevUtils

import Control.Monad.IO.Class
import Data.Maybe
import Snap.Types
import System.Directory
import System.FilePath
import Text.Html hiding ((</>))

handler :: Snap ()
handler = liftIO mkProjPage >>= htmlResponse
    

mkProjPage :: IO Html
mkProjPage = do
    projPath <- getCurrentDirectory
    let projName = takeFileName projPath
    projTree <- loadTree
    return $ devpage ("Project " ++ projName)
        [ h1 << projName
        , p << small << projPath
        , fileList projTree
        ]
        []
        []

fileList :: [SrcTree] -> Html
fileList tree = table ! [identifier "filelist"] <<
                    stripe (concatMap mkEntry tree)
  where
    stripe = zipWith (\c e -> e ! [theclass c]) (cycle ["even", "odd"])
    
    mkEntry (SrcTree si sub) = mkItem si : concatMap mkEntry sub

    mkItem si = tr << map col
        [ ("path", Just $ p << [ dirPart, namePart ])
        , ("op", previewLink si)
        , ("op", editLink si)
        , ("op", downloadLink si)
        , ("op", fileLink si)
        ]
      where
        path = srcPath si
        dirPart = let d = takeDirectory path
                      e = addTrailingPathSeparator d in
            if null d then noHtml else thespan ! [theclass "dir"] << e
        namePart = thespan ! [theclass $ "name " ++ fileType]
                        << takeFileName path
        fileType = case srcClass si of
                    SCPage -> "src-page"
                    SCImage -> "src-image"
                    SCScript -> "src-script"
                    SCText -> "src-text"
                    SCOther -> "src-other"
                    SCDir -> "src-dir"
        col (c, h) = td ! [theclass c] << fromMaybe spaceHtml h
    
{- add links for:
    add
    
    rename
    move
    delete
-}


data SrcTree = SrcTree SrcInfo [SrcTree]

loadTree :: IO [SrcTree]
loadTree = do
    buildSub ""
  where
    buildSub :: FilePath -> IO [SrcTree]
    buildSub root = do
        names <- getDirectoryContents (if null root then "." else root)
        let names' = filter okName names
        sequence $ map (buildEntry root) names'
    buildEntry :: FilePath -> FilePath -> IO SrcTree
    buildEntry root name = do
        let path = if null root then name else root </> name
        si <- getSrcInfo path
        sub <- if srcClass si == SCDir then buildSub path else return []
        return $ SrcTree si sub
    okName "" = False
    okName ('.':_) = False
    okName _ = True
    
        