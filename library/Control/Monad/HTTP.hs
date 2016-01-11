module Control.Monad.HTTP (
    MonadHTTP(..),
) where

import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Conduit as HTTP

class Monad m => MonadHTTP m where
    request :: HTTP.Request -> HTTP.Manager -> m (HTTP.Response LBS.ByteString)

instance MonadHTTP IO where
    request = HTTP.httpLbs
