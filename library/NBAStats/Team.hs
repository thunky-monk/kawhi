{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NBAStats.Team (
    misc,
    Misc(..),
    teams,
    Team(..),
    traditional,
    Traditional(..)
) where

import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.HTTP as MonadHTTP
import qualified Control.Monad.Trans as Trans
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Aeson ((.:))
import qualified Data.ByteString as SBS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified NBAStats
import qualified Network.HTTP.Conduit as HTTP

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

teams :: (Trans.MonadIO i, Catch.MonadCatch i, MonadHTTP.MonadHTTP i, Catch.MonadThrow m) => HTTP.Manager -> i (m [Team])
teams = NBAStats.stats path result (HashMap.toList defaultParameters)

data Traditional = Traditional {
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

instance Aeson.FromJSON Traditional where
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
        return Traditional {..}
    parseJSON invalid = Aeson.typeMismatch "Traditional" invalid

traditional :: (Trans.MonadIO i, Catch.MonadCatch i, MonadHTTP.MonadHTTP i, Catch.MonadThrow m) => Team -> HTTP.Manager -> i (m Traditional)
traditional team = NBAStats.stat path result "TEAM_ID" (identifier team) (HashMap.toList defaultParameters)

data Misc = Misc {
    pointsOffTurnovers :: Double,
    secondChancePoints :: Double,
    fastBreakPoints :: Double,
    pointsInThePaint :: Double
} deriving (Show, Eq)

instance Aeson.FromJSON Misc where
    parseJSON (Aeson.Object o) = do
        pointsOffTurnovers <- o .: "PTS_OFF_TOV"
        secondChancePoints <- o .: "PTS_2ND_CHANCE"
        fastBreakPoints <- o .: "PTS_FB"
        pointsInThePaint <- o .: "PTS_PAINT"
        return Misc {..}
    parseJSON invalid = Aeson.typeMismatch "Misc" invalid

misc :: (Trans.MonadIO i, Catch.MonadCatch i, MonadHTTP.MonadHTTP i, Catch.MonadThrow m) => Team -> HTTP.Manager -> i (m Misc)
misc team =
    let
        miscParams = HashMap.adjust (const $ Just "Misc") "MeasureType" defaultParameters
    in
        NBAStats.stat path result "TEAM_ID" (identifier team) (HashMap.toList miscParams)

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
