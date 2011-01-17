module EchoRequest where

import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Snap.Types
import Text.Html

handler :: Snap ()
handler = do
    req <- getRequest
    modifyResponse $ setContentType (C.pack "text/html; charset=UTF-8")
    writeBS $ (T.encodeUtf8 . T.pack) $ renderHtml (html req)
  where
    html req = 
        header << thetitle << "Echo Request" +++
        body << pre << show req

