module ListT.HTTPClient where

import BasePrelude hiding (cons, uncons)
import MTLPrelude
import ListT
import Data.ByteString (ByteString)
import qualified Network.HTTP.Client as HC
import qualified Data.ByteString as BS


type Stream =
  ListT IO ByteString


requestBody :: HC.Manager -> HC.Request -> IO Stream
requestBody manager request =
  HC.withResponse request manager $ \response -> return $ bodyReader $ HC.responseBody response

bodyReader :: HC.BodyReader -> Stream
bodyReader io =
  fix $ \loop -> do
    chunk <- lift io
    if BS.null chunk
      then empty
      else cons chunk loop
