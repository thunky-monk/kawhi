{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
    Copyright: Aaron Taylor, 2016
    License: MIT
    Maintainer: aaron@hamsterdam.co

    Class, instances and transformer for monads capable of HTTP requests.

    In some cases, it is useful to generalize this capability. For example, it can be used provide mock responses for testing.
-}
module Control.Monad.HTTP (
    -- * Class
    MonadHTTP(..),

    -- * Transformer
    HTTPT(..),
    runHTTPT
) where

import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans as Trans
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Simple as HTTPSimple
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP

{-|
    The class of monads capable of HTTP requests.
-}
class Monad m => MonadHTTP m where
    performRequest :: HTTP.Request -> m (HTTP.Response LBS.ByteString)

instance MonadHTTP IO where
    performRequest = HTTPSimple.httpLbs

instance Catch.MonadThrow m => MonadHTTP (HTTPT m) where
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

{-|
    An HTTP transformer monad parameterized by an inner monad 'm'.
-}
newtype HTTPT m a = HTTPT { unHTTPT :: Reader.ReaderT (HTTP.Response LBS.ByteString) m a }
    deriving (Functor, Applicative, Monad, Trans.MonadTrans, Catch.MonadThrow, Catch.MonadCatch, Trans.MonadIO, Reader.MonadReader (HTTP.Response LBS.ByteString))

{-|
    Run an HTTP monad action and extract the inner monad.
-}
runHTTPT ::
    HTTPT m a -- ^ The HTTP monad transformer
    -> HTTP.Response LBS.ByteString -- ^ The response
    -> m a -- ^ The resulting inner monad
runHTTPT = Reader.runReaderT . unHTTPT

instance Except.MonadError e m => Except.MonadError e (HTTPT m) where
    throwError = Trans.lift . Except.throwError
    catchError m f = HTTPT . Reader.ReaderT $ \r -> Except.catchError
        (runHTTPT m r)
        (\e -> runHTTPT (f e) r)
