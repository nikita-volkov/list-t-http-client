module ListT.HTTPClient where

import BasePrelude hiding (cons, uncons)
import MTLPrelude
import ListT
import Data.ByteString (ByteString)
import qualified Network.HTTP.Client as HC
import qualified Data.ByteString as BS


type Stream =
  ListT IO ByteString


requestBody :: (MonadReader HC.Manager m, MonadIO m) => HC.Request -> m Stream
requestBody request =
  do
    manager <- ask
    liftIO $ HC.withResponse request manager $ \response -> return $ bodyReader $ HC.responseBody response

bodyReader :: HC.BodyReader -> Stream
bodyReader io =
  fix $ \loop -> do
    chunk <- lift io
    if BS.null chunk
      then empty
      else cons chunk loop
