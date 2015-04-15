module ListT.HTTPClient where

import BasePrelude hiding (cons, uncons)
import MTLPrelude
import ListT
import Data.ByteString (ByteString)
import qualified Network.HTTP.Client as HC
import qualified Data.ByteString as B


withResponse :: HC.Request -> HC.Manager -> (HC.Response (ListT IO ByteString) -> IO a) -> IO a
withResponse request manager handler = 
  HC.withResponse request manager $ handler . fmap bodyReader

bodyReader :: HC.BodyReader -> ListT IO ByteString
bodyReader io =
  fix $ \loop -> lift io >>= \chunk -> if B.null chunk then mzero else cons chunk loop
