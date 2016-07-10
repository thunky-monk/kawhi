module Main where

import qualified Team

main :: IO ()
main = do
    eitherErrorOrTeams <- Team.teams
    case eitherErrorOrTeams of
        Left teamsError -> print teamsError
        Right teams -> do
            eitherErrorOrSplits <- fmap sequence $ traverse (\team -> Team.split team) teams
            case eitherErrorOrSplits of
                Left splitsError -> print splitsError
                Right splits -> do
                        let teamSplits = zip teams splits
                        print teamSplits
