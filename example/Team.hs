{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Team (
    teams,
    Team(..),
    split,
    Split(..)
) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Aeson ((.:))
import qualified Data.ByteString as SBS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified NBA.Stats as Stats

path :: SBS.ByteString
path = "leaguedashteamstats"

result :: Text.Text
result = "LeagueDashTeamStats"

data Team = Team {
    identifier :: Integer,
    name :: Text.Text
} deriving (Show, Eq)

instance Aeson.FromJSON Team where
    parseJSON (Aeson.Object o) = do
        identifier <- o .: "TEAM_ID"
        name <- o .: "TEAM_NAME"
        return Team {..}
    parseJSON invalid = Aeson.typeMismatch "Team" invalid

teams :: IO (Either Stats.StatsError [Team])
teams = Stats.getSplitRows path result (HashMap.toList defaultParameters)

data Split = Split {
    gamesPlayed :: Integer,
    minutesPlayed :: Double,
    fieldGoalsMade :: Double,
    fieldGoalsAttempted :: Double,
    threePointFieldGoalsMade :: Double,
    threePointFieldGoalsAttempted :: Double,
    freeThrowsMade :: Double,
    freeThrowsAttempted :: Double,
    offensiveRebounds ::  Double,
    defensiveRebounds :: Double,
    assists :: Double,
    turnovers :: Double,
    steals :: Double,
    blocks :: Double,
    blockedFieldGoalAttempts :: Double,
    personalFouls :: Double,
    personalFoulsDrawn :: Double,
    points :: Double,
    plusMinus :: Double
} deriving (Show, Eq)

instance Aeson.FromJSON Split where
    parseJSON (Aeson.Object o) = do
        gamesPlayed <- o .: "GP"
        minutesPlayed <- o .: "MIN"
        fieldGoalsMade <- o .: "FGM"
        fieldGoalsAttempted <- o .: "FGA"
        threePointFieldGoalsMade <- o .: "FG3M"
        threePointFieldGoalsAttempted <- o .: "FG3A"
        freeThrowsMade <- o .: "FTM"
        freeThrowsAttempted <- o .: "FTA"
        offensiveRebounds <- o .:  "OREB"
        defensiveRebounds <- o .: "DREB"
        assists <- o .: "AST"
        turnovers <- o .: "TOV"
        steals <- o .: "STL"
        blocks <- o .: "BLK"
        blockedFieldGoalAttempts <- o .: "BLKA"
        personalFouls <- o .: "PF"
        personalFoulsDrawn <- o .: "PFD"
        points <- o .: "PTS"
        plusMinus <- o .: "PLUS_MINUS"
        return Split {..}
    parseJSON invalid = Aeson.typeMismatch "Split" invalid

split :: Team -> IO (Either Stats.StatsError Split)
split team = Stats.getSplitRow path result "TEAM_ID" (identifier team) (HashMap.toList defaultParameters)

defaultParameters :: HashMap.HashMap SBS.ByteString (Maybe SBS.ByteString)
defaultParameters = HashMap.fromList [
    ("Conference", Nothing),
    ("DateFrom", Nothing),
    ("DateTo", Nothing),
    ("Division", Nothing),
    ("GameScope", Nothing),
    ("GameSegment", Nothing),
    ("LastNGames", Just "0"),
    ("LeagueID", Just "00"),
    ("Location", Nothing),
    ("MeasureType", Just "Base"),
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
    ("TeamID", Just "0"),
    ("VsConference", Nothing),
    ("VsDivision", Nothing)]
