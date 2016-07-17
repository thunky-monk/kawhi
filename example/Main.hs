{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Aeson ((.:))
import qualified NBA.Stats as Stats

main :: IO ()
main = do
    eitherErrorOrStats <- advancedStatsByMonth
    case eitherErrorOrStats of
        Left statsError -> print statsError
        Right stats -> print stats

data AdvancedStats = AdvancedStats {
    month :: String,
    offensiveRating :: Double,
    defensiveRating :: Double
} deriving (Show, Eq)

instance Aeson.FromJSON AdvancedStats where
    parseJSON (Aeson.Object o) = do
        month <- o .: "SEASON_MONTH_NAME"
        offensiveRating <- o .: "OFF_RATING"
        defensiveRating <- o .: "DEF_RATING"
        return AdvancedStats {..}
    parseJSON invalid = Aeson.typeMismatch "AdvancedStats" invalid

advancedStatsByMonth :: IO (Either Stats.StatsError [AdvancedStats])
advancedStatsByMonth = Stats.getSplitRows "teamdashboardbygeneralsplits" "MonthTeamDashboard"
    [
        ("Conference", Nothing),
        ("DateFrom", Nothing),
        ("DateTo", Nothing),
        ("Division", Nothing),
        ("GameScope", Nothing),
        ("GameSegment", Nothing),
        ("LastNGames", Just "0"),
        ("LeagueID", Just "00"),
        ("Location", Nothing),
        ("MeasureType", Just "Advanced"),
        ("Month", Just "0"),
        ("OpponentTeamID", Just "0"),
        ("Outcome", Nothing),
        ("PaceAdjust", Just "N"),
        ("PerMode", Just "PerGame"),
        ("Period", Just "0"),
        ("PlayerExperience", Nothing),
        ("PlayerPosition", Nothing),
        ("PlusMinus", Just "N"),
        ("PORound", Just "0"),
        ("Rank", Just "N"),
        ("Season", Just "2015-16"),
        ("SeasonSegment", Nothing),
        ("SeasonType", Just "Regular Season"),
        ("ShotClockRange", Nothing),
        ("StarterBench", Nothing),
        ("TeamID", Just "1610612759"),
        ("VsConference", Nothing),
        ("VsDivision", Nothing)
    ]
