{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.HTTP (
    MockHTTP,
    MonadHTTP(..),
    runMockHTTP
) where

import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans as Trans
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types as HTTP

class Monad m => MonadHTTP m where
    request :: HTTP.Request -> HTTP.Manager -> m (HTTP.Response LBS.ByteString)

instance MonadHTTP IO where
    request = HTTP.httpLbs

newtype MockHTTP m a = MockHTTP (Reader.ReaderT (HTTP.Response LBS.ByteString) m a)
    deriving (Applicative, Functor, Monad, Trans.MonadTrans, Catch.MonadThrow, Catch.MonadCatch, Trans.MonadIO, Reader.MonadReader (HTTP.Response LBS.ByteString))

runMockHTTP :: MockHTTP m a -> HTTP.Response LBS.ByteString -> m a
runMockHTTP (MockHTTP reader) = Reader.runReaderT reader

instance Catch.MonadThrow m => MonadHTTP (MockHTTP m) where
    request _ _ = check
        where
            check = do
                response <- Reader.ask
                let status = HTTP.responseStatus response
                if status >= HTTP.ok200 && status < HTTP.multipleChoices300
                    then return response
                    else Catch.throwM $ HTTP.StatusCodeException status [] (HTTP.createCookieJar [])
