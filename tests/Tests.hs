import qualified Test.Tasty as Tasty

import qualified Data.NBA.Stats.Tests as Stats

main :: IO ()
main = Tasty.defaultMain Stats.tests
