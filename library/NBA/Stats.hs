{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module NBA.Stats (
    Column,
    domain,
    getRequest,
    Parameters,
    Path,
    Stats(..),
    Split(..),
    SplitName,
    Row,
    getSplitRow,
    getSplitRows,
    StatsException(..)
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

getSplitRows :: (Trans.MonadIO m, Catch.MonadCatch m, MonadHTTP.MonadHTTP m, Aeson.FromJSON a) => Path -> SplitName -> Parameters -> HTTP.Manager -> m [a]
getSplitRows path splitName params manager = do
    response <- get path params manager
    split <- findSplit response splitName
    Monad.forM (rows split) $ convertTable (columns split)

getSplitRow :: (Trans.MonadIO m, Catch.MonadCatch m, MonadHTTP.MonadHTTP m, Eq v, Show v, Aeson.FromJSON v, Aeson.FromJSON a) => Path -> SplitName -> Column -> v -> Parameters -> HTTP.Manager -> m a
getSplitRow path splitName key value params manager = do
    response <- get path params manager
    split <- findSplit response splitName
    keyIndex <- maybe
        (Catch.throwM $ NoKeyInColumns $ Text.unpack key)
        return
        (List.elemIndex key (columns split))
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
            (rows split))
    convertTable (columns split) row

type Column = Text.Text

type Row = [Aeson.Value]

type Parameters = [(SBS.ByteString, Maybe SBS.ByteString)]

type Path = SBS.ByteString

type SplitName = Text.Text

data Split = Split {
    name :: SplitName,
    columns :: [Column],
    rows :: [Row]
} deriving (Show, Eq)

instance Aeson.FromJSON Split where
    parseJSON (Aeson.Object v) = do
        name <- v .: "name"
        columns <- v .: "headers"
        rows <- v .: "rowSet"
        return Split {..}
    parseJSON invalid = Aeson.typeMismatch "Split" invalid

instance Aeson.ToJSON Split where
    toJSON Split {..} = Aeson.object [
        "name" .= name,
        "headers" .= columns,
        "rowSet" .= rows]

data Stats = Stats {
    splits :: [Split]
} deriving (Show, Eq)

instance Aeson.ToJSON Stats where
    toJSON Stats {..} = Aeson.object [
        "resultSets" .= splits]

instance Aeson.FromJSON Stats where
    parseJSON (Aeson.Object o) = do
        splits <- o .: "resultSets"
        return Stats {..}
    parseJSON invalid = Aeson.typeMismatch "Stats" invalid

convertTable :: (Catch.MonadThrow m, Aeson.FromJSON a) => [Column] -> Row -> m a
convertTable columns row = do
    object <- fmap (Aeson.Object . fst) $ Foldable.foldlM
        (\(hash, index) column -> do
            value <- maybe
                (Catch.throwM $ NoValueForRowIndex $ show index)
                return
                (Safe.atMay row index)
            return (HashMap.insert column value hash, index `seq` index + 1))
        (HashMap.empty, 0)
        columns
    case Aeson.parse Aeson.parseJSON object of
        Aeson.Error message -> Catch.throwM $ TableConversionError message
        Aeson.Success split -> return split

findSplit :: Catch.MonadThrow m => HTTP.Response LBS.ByteString -> SplitName -> m Split
findSplit response splitName = do
    stats <- either
        (Catch.throwM . PayloadDecodeError)
        return
        (Aeson.eitherDecode . HTTP.responseBody $ response)
    maybe
        (Catch.throwM $ NoMatchingSplit $ Text.unpack splitName)
        return
        (List.find (\r -> name r == splitName) $ splits stats)

getRequest :: Trans.MonadIO m => Path -> m HTTP.Request
getRequest path = do
    initRequest <- Trans.liftIO (Default.def :: IO HTTP.Request)
    return initRequest {
        HTTP.method = "GET",
        HTTP.secure = False,
        HTTP.host = domain,
        HTTP.path = "/stats/" <> path
    }

get :: (Trans.MonadIO m, Catch.MonadCatch m, MonadHTTP.MonadHTTP m) => Path -> Parameters -> HTTP.Manager -> m (HTTP.Response LBS.ByteString)
get path params manager = do
    initRequest <- getRequest path
    let request = HTTP.setQueryString params initRequest
    MonadHTTP.performRequest request manager

data StatsException =
    PayloadDecodeError String |
    NoMatchingSplit String |
    NoMatchingRow String |
    NoValueForRowIndex String |
    NoKeyInColumns String |
    TableConversionError String
    deriving (Typeable.Typeable, Eq)

instance Show StatsException where
    show nbaException = "StatsException (" ++ showCase nbaException ++ ")"
        where
            showCase exception = case exception of
                PayloadDecodeError message -> format "PayloadDecodeError" message
                NoMatchingSplit message -> format "NoMatchingSplit" message
                NoMatchingRow message -> format "NoMatchingRow" message
                NoValueForRowIndex message -> format "NoValueForRowIndex" message
                NoKeyInColumns message -> format "NoKeyInColumns" message
                TableConversionError message -> format "TableConversionError" message
            format :: String -> String -> String
            format name message = name ++ " " ++ message

instance Catch.Exception StatsException
