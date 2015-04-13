module ListT.HTTPClient where

import BasePrelude hiding (cons, uncons)
import MTLPrelude
import ListT
import Data.ByteString (ByteString)
import qualified Network.HTTP.Client as HC
import qualified Data.ByteString as BS


requestBody :: (MonadReader HC.Manager m, MonadIO m, 
                MonadCons m', MonadIO m') => 
               HC.Request -> m (m' ByteString)
requestBody request =
  do
    manager <- ask
    liftIO $ HC.withResponse request manager $ \response -> return $ bodyReader $ HC.responseBody response

bodyReader :: (MonadCons m, MonadIO m) => HC.BodyReader -> m ByteString
bodyReader io =
  fix $ \loop -> do
    chunk <- liftIO io
    if BS.null chunk
      then mzero
      else cons chunk loop
