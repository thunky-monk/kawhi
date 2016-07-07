module Main where

import qualified Network.HTTP.Conduit as HTTP
import qualified Team

main :: IO ()
main = do
    manager <- HTTP.newManager HTTP.tlsManagerSettings
    eitherErrorOrTeams <- Team.teams manager
    case eitherErrorOrTeams of
        Left teamsError -> print teamsError
        Right teams -> do
            eitherErrorOrSplits <- fmap sequence $ traverse (\team -> Team.split team manager) teams
            case eitherErrorOrSplits of
                Left splitsError -> print splitsError
                Right splits -> do
                        let teamSplits = zip teams splits
                        print teamSplits
