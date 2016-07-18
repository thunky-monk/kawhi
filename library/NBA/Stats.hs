{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-|
    Module: NBA.Stats
    Copyright: Aaron Taylor, 2016
    License: MIT
    Maintainer: aaron@hamsterdam.co

    Functions and types for interacting with <http://stats.NBA.com NBA Stats>.
-}
module NBA.Stats (
    -- * How to use this library
    -- $use

    -- * Simple API
    getSplitRows,
    getSplitRow,

    -- * Generic API
    getSplitRowsGeneric,
    getSplitRowGeneric,

    -- * Types
    Stats(..),
    Split(..),
    SplitName,
    SplitColumn,
    SplitRow,
    StatsPath,
    StatsParameters,
    StatsError(..),

    -- * Utility
    domain,
    getRequest,
) where

import qualified Control.Monad as Monad
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Trans as Trans
import qualified Control.Monad.Http as MonadHttp
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

{- |
    Gets all the rows in a NBA Stats split.

    When using this function in a custom monad transformer, it may be desirable to use the generic version of this function, 'getSplitRowsGeneric', instead.
-}
getSplitRows ::
    (Aeson.FromJSON a)
    => StatsPath -- ^ The URL path for the stats web page containing the split.
    -> SplitName -- ^ The split name.
    -> StatsParameters -- ^ The parameters for customizing the stats.
    -> IO (Either StatsError [a]) -- ^ The return value: an IO action resulting in an error or split rows.
getSplitRows path splitName params = Except.runExceptT $ getSplitRowsGeneric path splitName params

{- |
    Gets a row in a NBA Stats split.

    When using this function in a custom monad transformer, it may be desirable to use the generic version of this function, 'getSplitRowGeneric', instead.
-}
getSplitRow ::
    (Eq v, Show v, Aeson.FromJSON v, Aeson.FromJSON a)
    => StatsPath -- ^ The URL path for the stats web page containing the split.
    -> SplitName -- ^ The split name.
    -> SplitColumn -- ^ The column name key for a the desired row.
    -> v -- ^ The expected row value associated with the column name key for a the desired row.
    -> StatsParameters -- ^ The parameters for customizing the stats.
    -> IO (Either StatsError a) -- ^ The return value: an IO action resulting in an error or a split row.
getSplitRow path splitName key value params = Except.runExceptT $ getSplitRowGeneric path splitName key value params

{- |
    Gets all the rows in a NBA Stats split.

    The simpler version of this function, 'getSplitRows', has a concrete 'm'.
-}
getSplitRowsGeneric ::
    (Trans.MonadIO m, MonadHttp.MonadHttp m, Except.MonadError StatsError m, Aeson.FromJSON a)
    => StatsPath -- ^ The URL path for the stats web page containing the split.
    -> SplitName -- ^ The split name.
    -> StatsParameters -- ^ The parameters for customizing the stats.
    -> m [a] -- ^ The return value: an action resulting in an error or split rows.
getSplitRowsGeneric path splitName params = do
    response <- get path params
    split <- findSplit response splitName
    Monad.forM (rows split) $ convertTable (columns split)

{- |
    Gets a row in an NBA Stats split.

    The simpler version of this function, 'getSplitRows', has a concrete 'm'.
-}
getSplitRowGeneric ::
    (Trans.MonadIO m, MonadHttp.MonadHttp m, Except.MonadError StatsError m, Eq v, Show v, Aeson.FromJSON v, Aeson.FromJSON a)
    => StatsPath -- ^ The URL path for the stats web page containing the split.
    -> SplitName -- ^ The split name.
    -> SplitColumn -- ^ The column name key for a the desired row.
    -> v -- ^ The expected row value associated with the column name key for a the desired row.
    -> StatsParameters -- ^ The parameters for customizing the stats.
    -> m a -- ^ The return value: an action resulting in an error or a split row.
getSplitRowGeneric path splitName key value params = do
    response <- get path params
    split <- findSplit response splitName
    keyIndex <- maybe
        (Except.throwError $ SplitColumnNameNotFound $ Text.unpack key)
        return
        (List.elemIndex key (columns split))
    row <- maybe
        (Except.throwError $ SplitKeyNotFound $ show value)
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

{- |
    An NBA Stats split.

    This type represents splits available from NBA Stats.
-}
data Split =
    -- | Constructor for a split.
    Split {
        -- | The split's name.
        name :: SplitName,
        -- | The split's column names.
        columns :: [SplitColumn],
        -- | The split's rows of data.
        rows :: [SplitRow]
    }
    deriving (Show, Eq)

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

-- | An NBA Stats split name.
type SplitName = Text.Text

-- | A column name in an NBA Stats split.
type SplitColumn = Text.Text

-- | A row of data in an NBA Stats split.
type SplitRow = [Aeson.Value]

{- |
    An NBA Stats resource.

    This type represents the top-level JSON object returned from the NBA Stats REST API.
-}
data Stats =
    -- | Constructor for stats resource.
    Stats {
        -- | The resource's splits.
        splits :: [Split]
    }
    deriving (Show, Eq)

instance Aeson.ToJSON Stats where
    toJSON Stats {..} = Aeson.object [
        "resultSets" .= splits]

instance Aeson.FromJSON Stats where
    parseJSON (Aeson.Object o) = do
        splits <- o .: "resultSets"
        return Stats {..}
    parseJSON invalid = Aeson.typeMismatch "Stats" invalid

-- | A URL path for an NBA Stats resource.
type StatsPath = SBS.ByteString

-- | A collection of parameters that customize NBA Stats resources.
type StatsParameters = [(SBS.ByteString, Maybe SBS.ByteString)]

{- |
    An error which may be generated by this library.
-}
data StatsError =
    -- | An HTTP response has invalid JSON.
    StatsResponseDecodeFailure String |
    -- | A stats resource does not have a split matching the given split name.
    SplitNameNotFound String |
    -- | A split does not have a row matching the given column key and row value.
    SplitKeyNotFound String |
    -- | A split row has less values than columns.
    SplitRowValueNotFound String |
    -- | A split does not have a column name matching the given key.
    SplitColumnNameNotFound String |
    -- | A failure to parse a split row's tabular data to the destination type.
    SplitRowParseFailure String
    deriving (Eq)

instance Show StatsError where
    show statsError = "StatsError (" ++ showCase statsError ++ ")"
        where
            showCase err = case err of
                StatsResponseDecodeFailure message -> format "StatsResponseDecodeFailure" message
                SplitNameNotFound message -> format "SplitNameNotFound" message
                SplitKeyNotFound message -> format "SplitKeyNotFound" message
                SplitRowValueNotFound message -> format "SplitRowValueNotFound" message
                SplitColumnNameNotFound message -> format "SplitColumnNameNotFound" message
                SplitRowParseFailure message -> format "SplitRowParseFailure" message
            format :: String -> String -> String
            format name message = name ++ " " ++ message

{- |
    Generates an HTTP GET request like the ones used internally.
-}
getRequest :: StatsPath -> HTTP.Request
getRequest path = HTTP.defaultRequest {
    HTTP.method = "GET",
    HTTP.secure = False,
    HTTP.host = domain,
    HTTP.path = "/stats/" <> path
}

{- |
    The NBA Stats domain name.
-}
domain :: SBS.ByteString
domain = "stats.nba.com"

convertTable :: (Except.MonadError StatsError m, Aeson.FromJSON a) => [SplitColumn] -> SplitRow -> m a
convertTable columns row = do
    object <- fmap (Aeson.Object . fst) $ Foldable.foldrM
        (\column (hash, index) -> do
            value <- maybe
                (Except.throwError $ SplitRowValueNotFound $ show index)
                return
                (Safe.atMay row index)
            return (HashMap.insert column value hash, index `seq` index - 1))
        (HashMap.empty, length columns - 1)
        columns
    case Aeson.parse Aeson.parseJSON object of
        Aeson.Error message -> Except.throwError $ SplitRowParseFailure message
        Aeson.Success split -> return split

findSplit :: (Except.MonadError StatsError m) => HTTP.Response LBS.ByteString -> SplitName -> m Split
findSplit response splitName = do
    stats <- either
        (Except.throwError . StatsResponseDecodeFailure)
        return
        (Aeson.eitherDecode . HTTP.responseBody $ response)
    maybe
        (Except.throwError $ SplitNameNotFound $ Text.unpack splitName)
        return
        (List.find (\r -> name r == splitName) $ splits stats)



get :: (Trans.MonadIO m, MonadHttp.MonadHttp m) => StatsPath -> StatsParameters -> m (HTTP.Response LBS.ByteString)
get path params = MonadHttp.performRequest $ HTTP.setQueryString params $ getRequest path

{- $use
    @
    import Data.Aeson
    import Data.Aeson.Types
    import NBA.Stats

    data Team = Team {
        identifier :: Integer,
        name :: String
    } deriving (Show, Eq)

    instance FromJSON Team where
        parseJSON (Object o) = do
            identifier <- o .: \"TEAM_ID\"
            name <- o .: \"TEAM_NAME\"
            return Team {..}
        parseJSON invalid = typeMismatch \"Team\" invalid

    getTeams :: IO (Either StatsError [Team])
    getTeams = getSplitRows
        \"leaguedashteamstats\"
        \"LeagueDashTeamStats\"
        [(\"Conference\", Nothing), (\"PerMode\", Just \"PerGame\")]

    main :: IO ()
    main = do
        eitherErrorOrTeams <- getTeams
        case eitherErrorOrTeams of
            Left err -> print err
            Right teams -> print teams
    @

-}
