{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module NBA.Stats (
    Column,
    domain,
    getRequest,
    getSplitRow,
    getSplitRows,
    getSplitRowGeneric,
    getSplitRowsGeneric,
    Parameters,
    Path,
    Stats(..),
    Split(..),
    SplitName,
    Row,
    StatsError(..)
) where

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Trans as Trans
import qualified Control.Monad.HTTP as MonadHTTP
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Aeson ((.:), (.=))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as SBS
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import Data.Monoid ((<>))
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Text as Text
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Simple as HTTP
import qualified Safe

domain :: SBS.ByteString
domain = "stats.nba.com"

getSplitRows :: (Aeson.FromJSON a) => Path -> SplitName -> Parameters -> IO (Either StatsError [a])
getSplitRows path splitName params = Except.runExceptT $ getSplitRowsGeneric path splitName params

getSplitRowsGeneric :: (Trans.MonadIO m, MonadHTTP.MonadHTTP m, Except.MonadError StatsError m, Aeson.FromJSON a) => Path -> SplitName -> Parameters -> m [a]
getSplitRowsGeneric path splitName params = do
    response <- get path params
    split <- findSplit response splitName
    Monad.forM (rows split) $ convertTable (columns split)

getSplitRow :: (Eq v, Show v, Aeson.FromJSON v, Aeson.FromJSON a) => Path -> SplitName -> Column -> v -> Parameters -> IO (Either StatsError a)
getSplitRow path splitName key value params = Except.runExceptT $ getSplitRowGeneric path splitName key value params

getSplitRowGeneric :: (Trans.MonadIO m, MonadHTTP.MonadHTTP m, Except.MonadError StatsError m, Eq v, Show v, Aeson.FromJSON v, Aeson.FromJSON a) => Path -> SplitName -> Column -> v -> Parameters -> m a
getSplitRowGeneric path splitName key value params = do
    response <- get path params
    split <- findSplit response splitName
    keyIndex <- maybe
        (Except.throwError $ NoKeyInColumns $ Text.unpack key)
        return
        (List.elemIndex key (columns split))
    row <- maybe
        (Except.throwError $ NoMatchingRow $ show value)
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

convertTable :: (Except.MonadError StatsError m, Aeson.FromJSON a) => [Column] -> Row -> m a
convertTable columns row = do
    object <- fmap (Aeson.Object . fst) $ Foldable.foldlM
        (\(hash, index) column -> do
            value <- maybe
                (Except.throwError $ NoValueForRowIndex $ show index)
                return
                (Safe.atMay row index)
            return (HashMap.insert column value hash, index `seq` index + 1))
        (HashMap.empty, 0)
        columns
    case Aeson.parse Aeson.parseJSON object of
        Aeson.Error message -> Except.throwError $ TableConversionError message
        Aeson.Success split -> return split

findSplit :: (Except.MonadError StatsError m) => HTTP.Response LBS.ByteString -> SplitName -> m Split
findSplit response splitName = do
    stats <- either
        (Except.throwError . PayloadDecodeError)
        return
        (Aeson.eitherDecode . HTTP.responseBody $ response)
    maybe
        (Except.throwError $ NoMatchingSplit $ Text.unpack splitName)
        return
        (List.find (\r -> name r == splitName) $ splits stats)

getRequest :: Path -> HTTP.Request
getRequest path = HTTP.defaultRequest {
    HTTP.method = "GET",
    HTTP.secure = False,
    HTTP.host = domain,
    HTTP.path = "/stats/" <> path
}

get :: (Trans.MonadIO m, MonadHTTP.MonadHTTP m) => Path -> Parameters -> m (HTTP.Response LBS.ByteString)
get path params = MonadHTTP.performRequest $ HTTP.setQueryString params $ getRequest path

data StatsError =
    PayloadDecodeError String |
    NoMatchingSplit String |
    NoMatchingRow String |
    NoValueForRowIndex String |
    NoKeyInColumns String |
    TableConversionError String
    deriving (Eq)

instance Show StatsError where
    show statsError = "StatsError (" ++ showCase statsError ++ ")"
        where
            showCase err = case err of
                PayloadDecodeError message -> format "PayloadDecodeFailure" message
                NoMatchingSplit message -> format "NoMatchingSplit" message
                NoMatchingRow message -> format "NoMatchingRow" message
                NoValueForRowIndex message -> format "NoValueForRowIndex" message
                NoKeyInColumns message -> format "NoKeyInColumns" message
                TableConversionError message -> format "TableConversionFailure" message
            format :: String -> String -> String
            format name message = name ++ " " ++ message
