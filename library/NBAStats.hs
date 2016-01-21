{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module NBAStats (
    Column,
    domain,
    getRequest,
    NBAStatsException(..),
    Parameters,
    Path,
    Resource(..),
    Result(..),
    ResultName,
    Row,
    stat,
    stats
) where

import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.Trans as Trans
import qualified Control.Monad.HTTP as MonadHTTP
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Aeson ((.:), (.=))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as SBS
import qualified Data.Default as Default
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import Data.Monoid ((<>))
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Text as Text
import qualified Data.Typeable as Typeable
import qualified Network.HTTP.Conduit as HTTP
import qualified Safe

domain :: SBS.ByteString
domain = "stats.nba.com"

stats :: (Trans.MonadIO i, Catch.MonadCatch i, MonadHTTP.MonadHTTP i, Catch.MonadThrow m, Aeson.FromJSON a) => Path -> ResultName -> Parameters -> HTTP.Manager -> i (m [a])
stats path resultName params manager = do
    eitherResponse <- catchHTTP $ get path params manager
    return $ do
        result <- findResult eitherResponse resultName
        Monad.forM (rows result) $ convertTable (columns result)

stat :: (Trans.MonadIO i, Catch.MonadCatch i, MonadHTTP.MonadHTTP i, Catch.MonadThrow m, Eq v, Show v, Aeson.FromJSON v, Aeson.FromJSON a) => Path -> ResultName -> Column -> v -> Parameters -> HTTP.Manager -> i (m a)
stat path resultName key value params manager = do
    eitherResponse <- catchHTTP $ get path params manager
    return $ do
        result <- findResult eitherResponse resultName
        keyIndex <- maybe
            (Catch.throwM $ NoKeyInColumns $ Text.unpack key)
            return
            (List.elemIndex key (columns result))
        row <- maybe
            (Catch.throwM $ NoMatchingRow $ show value)
            return
            (List.find
                (\row ->
                    case Safe.atMay row keyIndex of
                        Nothing -> False
                        Just v -> case Aeson.parseMaybe Aeson.parseJSON v of
                            Nothing -> False
                            Just a -> a == value)
                (rows result))
        convertTable (columns result) row

type Column = Text.Text

type Row = [Aeson.Value]

type Parameters = [(SBS.ByteString, Maybe SBS.ByteString)]

type Path = SBS.ByteString

type ResultName = Text.Text

data Result = Result {
    name :: ResultName,
    columns :: [Column],
    rows :: [Row]
} deriving (Show, Eq)

instance Aeson.FromJSON Result where
    parseJSON (Aeson.Object v) = do
        name <- v .: "name"
        columns <- v .: "headers"
        rows <- v .: "rowSet"
        return Result {..}
    parseJSON invalid = Aeson.typeMismatch "Result" invalid

instance Aeson.ToJSON Result where
    toJSON Result {..} = Aeson.object [
        "name" .= name,
        "headers" .= columns,
        "rowSet" .= rows]

data Resource = Resource {
    results :: [Result]
} deriving (Show, Eq)

instance Aeson.ToJSON Resource where
    toJSON Resource {..} = Aeson.object [
        "resultSets" .= results]

instance Aeson.FromJSON Resource where
    parseJSON (Aeson.Object o) = do
        results <- o .: "resultSets"
        return Resource {..}
    parseJSON invalid = Aeson.typeMismatch "Resource" invalid

convertTable :: (Catch.MonadThrow m, Aeson.FromJSON a) => [Column] -> Row -> m a
convertTable columns row = do
    object <- Monad.liftM (Aeson.Object . fst) $ Foldable.foldlM
        (\(hash, index) column -> do
            value <- maybe
                (Catch.throwM $ NoValueForRowIndex $ show index)
                return
                (Safe.atMay row index)
            return (HashMap.insert column value hash, index + 1))
        (HashMap.empty, 0)
        columns
    case Aeson.parse Aeson.parseJSON object of
        Aeson.Error message -> Catch.throwM $ TableConversionError message
        Aeson.Success result -> return result

findResult :: Catch.MonadThrow m => m (HTTP.Response LBS.ByteString) -> ResultName -> m Result
findResult eitherResponse resultName = do
    response <- eitherResponse
    resource <- either
        (Catch.throwM . PayloadDecodeError)
        return
        (Aeson.eitherDecode . HTTP.responseBody $ response)
    maybe
        (Catch.throwM $ NoMatchingResult $ Text.unpack resultName)
        return
        (List.find (\r -> name r == resultName) $ results resource)

getRequest :: Trans.MonadIO m => Path -> m HTTP.Request
getRequest path = do
    initRequest <- Trans.liftIO (Default.def :: IO HTTP.Request)
    return initRequest {
        HTTP.method = "GET",
        HTTP.secure = False,
        HTTP.host = domain,
        HTTP.path = "/stats/" <> path
    }

get :: (Trans.MonadIO i, Catch.MonadCatch i, MonadHTTP.MonadHTTP i, Catch.MonadThrow m) => Path -> Parameters -> HTTP.Manager -> i (m (HTTP.Response LBS.ByteString))
get path params manager = do
    initRequest <- getRequest path
    let request = HTTP.setQueryString params initRequest
    catchHTTP $ Monad.liftM return (MonadHTTP.performRequest request manager)

catchHTTP :: (Trans.MonadIO m, Catch.MonadCatch m) => m a -> m a
catchHTTP f =
    Catch.catch
        f
        (\(e :: HTTP.HttpException) -> Catch.throwM . HTTPException $ show e)

data NBAStatsException =
    HTTPException String |
    PayloadDecodeError String |
    NoMatchingResult String |
    NoMatchingRow String |
    NoValueForRowIndex String |
    NoKeyInColumns String |
    TableConversionError String
    deriving (Typeable.Typeable, Eq)

instance Show NBAStatsException where
    show nbaException = "NBAStatsException (" ++ showCase nbaException ++ ")"
        where
            showCase exception = case exception of
                HTTPException message -> format "HTTPException" message
                PayloadDecodeError message -> format "PayloadDecodeError" message
                NoMatchingResult message -> format "NoMatchingResult" message
                NoMatchingRow message -> format "NoMatchingRow" message
                NoValueForRowIndex message -> format "NoValueForRowIndex" message
                NoKeyInColumns message -> format "NoKeyInColumns" message
                TableConversionError message -> format "TableConversionError" message
            format :: String -> String -> String
            format name message = name ++ " " ++ message

instance Catch.Exception NBAStatsException
