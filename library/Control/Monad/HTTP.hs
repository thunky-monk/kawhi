{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.HTTP (
    MockHTTP,
    MonadHTTP(..),
    runMockHTTP
) where

import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans as Trans
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Simple as HTTPSimple
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP

class Monad m => MonadHTTP m where
    performRequest :: HTTP.Request -> m (HTTP.Response LBS.ByteString)

instance MonadHTTP IO where
    performRequest = HTTPSimple.httpLbs

newtype MockHTTP m a = MockHTTP (Reader.ReaderT (HTTP.Response LBS.ByteString) m a)
    deriving (Applicative, Functor, Monad, Trans.MonadTrans, Catch.MonadThrow, Catch.MonadCatch, Trans.MonadIO, Reader.MonadReader (HTTP.Response LBS.ByteString))

runMockHTTP :: MockHTTP m a -> HTTP.Response LBS.ByteString -> m a
runMockHTTP (MockHTTP reader) = Reader.runReaderT reader

mockHTTP :: (HTTP.Response LBS.ByteString -> m a) -> MockHTTP m a
mockHTTP r = MockHTTP $ Reader.ReaderT r

instance Catch.MonadThrow m => MonadHTTP (MockHTTP m) where
    performRequest _ = check
        where
            check = do
                response <- Reader.ask
                let status = HTTP.responseStatus response
                if status >= HTTP.ok200 && status < HTTP.multipleChoices300
                    then return response
                    else Catch.throwM $ HTTP.StatusCodeException status [] (HTTP.createCookieJar [])

instance Trans.MonadIO m => MonadHTTP (Except.ExceptT e m) where
    performRequest = HTTPSimple.httpLbs

instance Except.MonadError e m => Except.MonadError e (MockHTTP m) where
    throwError = Trans.lift . Except.throwError
    catchError m f = mockHTTP $ \r -> Except.catchError
        (runMockHTTP m r)
        (\e -> runMockHTTP (f e) r)
