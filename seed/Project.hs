module Project where

import DevUtils

import Control.Monad.IO.Class
import Data.Maybe
import Snap.Types
import System.Directory
import System.FilePath
import Text.Html hiding ((</>))

nu = () -- DO NOT DELETE THIS

handler :: Snap ()
handler = liftIO mkProjPage >>= htmlResponse
    

mkProjPage :: IO Html
mkProjPage = do
    projPath <- getCurrentDirectory
    let projName = takeFileName projPath
    projDir <- loadDir
    return $ devpage ("Project " ++ projName)
        [ h1 << projName
        , p << small << projPath
        , mkProjDir projDir
        ]
        []
        []

mkProjDir :: ProjDir -> Html
mkProjDir dir = table ! [identifier "filelist"] <<
                    stripe (concatMap mkEntry dir)
  where
    mkEntry pe = mkItem pe : concatMap mkEntry (peSub pe)
    mkItem pe = tr << td << p << [ dirPart pe, namePart pe ]
    dirPart pe = if null $ pePrefix pe
                     then noHtml
                     else thespan ! [theclass "dir"] << (pePrefix pe ++ sep)
    namePart pe = thespan ! [theclass "name"] << peName pe
    sep = [pathSeparator]
    stripe = zipWith (\c e -> e ! [theclass c]) (cycle ["even", "odd"])
    

data ProjEntry = ProjEntry { peFull :: FilePath
                           , pePrefix :: FilePath
                           , peName :: FilePath
                           , peSub :: ProjDir
                           }
type ProjDir = [ProjEntry]

loadDir :: IO ProjDir
loadDir = do
    cwd <- getCurrentDirectory
    buildSub cwd ""
  where
    buildSub :: FilePath -> FilePath -> IO ProjDir
    buildSub fp rp = do
        names <- getDirectoryContents fp
        let names' = catMaybes $ map legalPath names
        sequence $ map (buildEntry fp rp) names'
    buildEntry :: FilePath -> FilePath -> FilePath -> IO ProjEntry
    buildEntry fp rp name = do
        let fp' = fp </> name
        let rp' = rp </> name
        isDir <- doesDirectoryExist fp'
        sub <- if isDir then buildSub fp' rp' else return []
        return ProjEntry { peFull = rp'
                         , pePrefix = rp
                         , peName = name
                         , peSub = sub
                         }
        