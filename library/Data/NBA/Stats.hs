{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-|
    Module: Data.NBA.Stats
    Copyright: Aaron Taylor, 2016
    License: MIT
    Maintainer: aaron@hamsterdam.co

    Functions and types for interacting with <http://stats.NBA.com NBA Stats>.
-}
module Data.NBA.Stats (
    -- * How to use this library
    -- $use

    -- * Simple API
    getSplitRows,
    getSplitRow,
    splitRows,
    splitRow,

    -- * Generic API
    getSplitRowsGeneric,
    getSplitRowGeneric,
    splitRowsGeneric,
    splitRowGeneric,

    -- * Types
    Stats(..),
    Split(..),
    SplitName,
    SplitColumn,
    SplitRow,
    StatsPath,
    StatsParameters,
    StatsError(..),
) where

import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Trans as Trans
import qualified Control.Monad.Http as MonadHttp
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Aeson ((.:), (.=))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.List as List
import Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Simple as HTTP
import qualified Safe

{- |
    Gets all the rows in a NBA Stats split.

    To retrieve the raw data from NBA Stats independently from parsing, use 'splitRows'.

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
    Parses all the rows of an NBA Stats split from abitrary data.

    Alternatively, 'getSplitRows' retrieves the data from NBA Stats before parsing.

    To use something other than 'Either' for errors, use the generic version of this function, 'splitRowsGeneric', instead.
-}
splitRows ::
    Aeson.FromJSON a
    => SplitName -- ^ The split name.
    -> StatsBytes -- ^ The bytes to decode into split rows.
    -> Either StatsError [a] -- ^ The return value: an action resulting in an error or split rows.
splitRows = splitRowsGeneric

{- |
    Gets a row in a NBA Stats split.

    To retrieve the raw data from NBA Stats independently from parsing, use 'splitRows'.

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
    Parses a row of an NBA Stats split from abitrary data.

    Alternatively, 'getSplitRow' retrieves the data from NBA Stats before parsing.

    To use something other than 'Either' for errors, use the generic version of this function, 'splitRowGeneric', instead.
-}
splitRow ::
    (Eq v, Show v, Aeson.FromJSON v, Aeson.FromJSON a)
    => SplitName -- ^ The split name.
    -> SplitColumn -- ^ The column name key for a the desired row.
    -> v -- ^ The expected row value associated with the column name key for a the desired row.
    -> StatsBytes -- ^ The bytes to decode into a split row.
    -> Either StatsError a -- ^ The return value: an action resulting in an error or a split row.
splitRow = splitRowGeneric

{- |
    Gets all the rows in a NBA Stats split.

    To retrieve the raw data from NBA Stats independently from parsing, use 'splitRowsGeneric'.

    The simpler version of this function, 'getSplitRows', has a concrete 'm'.
-}
getSplitRowsGeneric ::
    (Trans.MonadIO m, MonadHttp.MonadHttp m, Except.MonadError StatsError m, Catch.MonadThrow m, Aeson.FromJSON a)
    => StatsPath -- ^ The URL path for the stats web page containing the split.
    -> SplitName -- ^ The split name.
    -> StatsParameters -- ^ The parameters for customizing the stats.
    -> m [a] -- ^ The return value: an action resulting in an error or split rows.
getSplitRowsGeneric path splitName params = get path params >>= splitRowsGeneric splitName

{- |
    Parses all the rows of an NBA Stats split from abitrary data.

    Alternatively, 'getSplitRowsGeneric' retrieves the data from NBA Stats before parsing.
-}
splitRowsGeneric ::
    (Except.MonadError StatsError m, Aeson.FromJSON a)
    => SplitName -- ^ The split name.
    -> StatsBytes -- ^ The bytes to decode into split rows.
    -> m [a] -- ^ The return value: an action resulting in an error or split rows.
splitRowsGeneric splitName bytes = do
    split <- findSplit splitName bytes
    traverse (parseSplitRow $ columns split) $ rows split

{- |
    Gets a row in an NBA Stats split.

    To retrieve the raw data from NBA Stats independently from parsing, use 'splitRowGeneric'.

    The simpler version of this function, 'getSplitRows', has a concrete 'm'.
-}
getSplitRowGeneric ::
    (Trans.MonadIO m, MonadHttp.MonadHttp m, Except.MonadError StatsError m, Catch.MonadThrow m, Eq v, Show v, Aeson.FromJSON v, Aeson.FromJSON a)
    => StatsPath -- ^ The URL path for the stats web page containing the split.
    -> SplitName -- ^ The split name.
    -> SplitColumn -- ^ The column name key for a the desired row.
    -> v -- ^ The expected row value associated with the column name key for a the desired row.
    -> StatsParameters -- ^ The parameters for customizing the stats.
    -> m a -- ^ The return value: an action resulting in an error or a split row.
getSplitRowGeneric path splitName key value params = get path params >>= splitRowGeneric splitName key value

{- |
    Parses a row of an NBA Stats split from abitrary data.

    Alternatively, 'getSplitRowGeneric' retrieves the data from NBA Stats before parsing.
-}
splitRowGeneric ::
    (Except.MonadError StatsError m, Eq v, Show v, Aeson.FromJSON v, Aeson.FromJSON a)
    => SplitName -- ^ The split name.
    -> SplitColumn -- ^ The column name key for a the desired row.
    -> v -- ^ The expected row value associated with the column name key for a the desired row.
    -> StatsBytes -- ^ The bytes to decode into a split row.
    -> m a -- ^ The return value: an action resulting in an error or a split row.
splitRowGeneric splitName key value bytes = do
    split <- findSplit splitName bytes
    keyIndex <- maybe
        (Except.throwError $ SplitColumnNameNotFound $ Text.unpack key)
        return
        (List.elemIndex key (columns split))
    row <- maybe
        (Except.throwError $ SplitKeyNotFound $ show value)
        return
        (List.find
            (\row -> maybe
                False
                (==value)
                (Safe.atMay row keyIndex >>= Aeson.parseMaybe Aeson.parseJSON))
            (rows split))
    parseSplitRow (columns split) row

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

-- | Bytes representing an NBA Stats resource.
type StatsBytes = LBS.ByteString

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
    -- | A split row has a different cardinality than the associated columns.
    SplitRowCardinalityInconsistent String |
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
                SplitRowCardinalityInconsistent message -> format "SplitRowCardinalityInconsistent" message
                SplitColumnNameNotFound message -> format "SplitColumnNameNotFound" message
                SplitRowParseFailure message -> format "SplitRowParseFailure" message
            format :: String -> String -> String
            format name message = name ++ " " ++ message

parseSplitRow :: (Except.MonadError StatsError m, Aeson.FromJSON a) => [SplitColumn] -> SplitRow -> m a
parseSplitRow columns row =
    if length columns == length row
        then case Aeson.parse Aeson.parseJSON $ Aeson.object (zip columns row) of
            Aeson.Error message -> Except.throwError $ SplitRowParseFailure message
            Aeson.Success split -> return split
        else Except.throwError $ SplitRowCardinalityInconsistent $ show row

findSplit :: (Except.MonadError StatsError m) => SplitName -> StatsBytes -> m Split
findSplit splitName bytes = do
    stats <- either
        (Except.throwError . StatsResponseDecodeFailure)
        return
        (Aeson.eitherDecode bytes)
    maybe
        (Except.throwError $ SplitNameNotFound $ Text.unpack splitName)
        return
        (List.find (\r -> name r == splitName) $ splits stats)



get :: (MonadHttp.MonadHttp m, Catch.MonadThrow m) => StatsPath -> StatsParameters -> m StatsBytes
get path params = HTTP.responseBody <$> getRequest
    where
      getRequest =
        modifyRequest
        <$> HTTP.parseRequest (Char8.unpack $ "http://stats.nba.com/stats/" <> path)
        >>= MonadHttp.performRequest
      modifyRequest =
        HTTP.setRequestHeaders [("Accept-Language","en-us"), ("Accept", "application/json")]
        . HTTP.setQueryString params

{- $use
    The following is a working example of getting some "advanced statistics", split by month, for the San Antonio Spurs 2015-2016 regular season.

    To learn how to find the NBA Stats values, like 'teamdashboardbygeneralsplits', for this example, read the <https://github.com/thunky-monk/kawhi/blob/master/guide.md guide>.

    @
    import qualified Data.Aeson as Aeson
    import qualified Data.Aeson.Types as Aeson
    import Data.Aeson ((.:))
    import qualified Data.NBA.Stats as Stats

    main :: IO ()
    main = do
        eitherErrorOrStats <- advancedStatsByMonth
        case eitherErrorOrStats of
            Left statsError -> print statsError
            Right stats -> mapM_ print stats

    data AdvancedStats = AdvancedStats {
        month :: String,
        offensiveRating :: Double,
        defensiveRating :: Double
    } deriving (Show, Eq)

    instance Aeson.FromJSON AdvancedStats where
        parseJSON (Aeson.Object o) = do
            month <- o .: \"SEASON_MONTH_NAME\"
            offensiveRating <- o .: \"OFF_RATING\"
            defensiveRating <- o .: \"DEF_RATING\"
            return AdvancedStats {..}
        parseJSON invalid = Aeson.typeMismatch \"AdvancedStats\" invalid

    advancedStatsByMonth :: IO (Either Stats.StatsError [AdvancedStats])
    advancedStatsByMonth = Stats.getSplitRows \"teamdashboardbygeneralsplits\" \"MonthTeamDashboard\"
        [
            (\"Conference\", Nothing),
            (\"DateFrom\", Nothing),
            (\"DateTo\", Nothing),
            (\"Division\", Nothing),
            (\"GameScope\", Nothing),
            (\"GameSegment\", Nothing),
            (\"LastNGames\", Just \"0\"),
            (\"LeagueID\", Just \"00\"),
            (\"Location\", Nothing),
            (\"MeasureType\", Just \"Advanced\"),
            (\"Month\", Just \"0\"),
            (\"OpponentTeamID\", Just \"0\"),
            (\"Outcome\", Nothing),
            (\"PaceAdjust\", Just \"N\"),
            (\"PerMode\", Just \"PerGame\"),
            (\"Period\", Just \"0\"),
            (\"PlayerExperience\", Nothing),
            (\"PlayerPosition\", Nothing),
            (\"PlusMinus\", Just \"N\"),
            (\"PORound\", Just \"0\"),
            (\"Rank\", Just \"N\"),
            (\"Season\", Just \"2015-16\"),
            (\"SeasonSegment\", Nothing),
            (\"SeasonType\", Just \"Regular Season\"),
            (\"ShotClockRange\", Nothing),
            (\"StarterBench\", Nothing),
            (\"TeamID\", Just \"1610612759\"),
            (\"VsConference\", Nothing),
            (\"VsDivision\", Nothing)
        ]
    @

    This program's output at the time of writing is:

    @
    AdvancedStats {month = \"October\", offensiveRating = 102.7, defensiveRating = 93.4}
    AdvancedStats {month = \"November\", offensiveRating = 102.5, defensiveRating = 93.4}
    AdvancedStats {month = \"December\", offensiveRating = 111.8, defensiveRating = 91.5}
    AdvancedStats {month = \"January\", offensiveRating = 114.0, defensiveRating = 100.7}
    AdvancedStats {month = \"February\", offensiveRating = 110.7, defensiveRating = 99.1}
    AdvancedStats {month = \"March\", offensiveRating = 107.8, defensiveRating = 97.2}
    AdvancedStats {month = \"April\", offensiveRating = 102.3, defensiveRating = 103.5}
    @
-}
