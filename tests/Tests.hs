import qualified Test.Tasty as Tasty

import qualified NBAStats.Tests as NBAStats

main :: IO ()
main = Tasty.defaultMain NBAStats.tests
