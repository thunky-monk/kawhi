import qualified Test.Tasty as Tasty

import qualified NBA.Stats.Tests as Stats

main :: IO ()
main = Tasty.defaultMain Stats.tests
