{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.NBA.Stats.Tests where

import qualified Control.Monad.Except as Except
import qualified Control.Monad.Http as MonadHttp
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Aeson ((.:))
import qualified Data.ByteString.Lazy as ByteString
import Data.Monoid ((<>))
import qualified Data.Scientific as Sci
import qualified Data.Text as Text
import qualified Network.HTTP.Client.Internal as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Data.NBA.Stats as Stats
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit ((@?=))

tests :: Tasty.TestTree
tests = Tasty.testGroup "Data.NBA.Stats" [
    propertyStatsErrorShow Stats.StatsResponseDecodeFailure "StatsResponseDecodeFailure",
    propertyStatsErrorShow Stats.SplitNameNotFound "SplitNameNotFound",
    propertyStatsErrorShow Stats.SplitKeyNotFound "SplitKeyNotFound",
    propertyStatsErrorShow Stats.SplitColumnNameNotFound "SplitColumnNameNotFound",
    propertyStatsErrorShow Stats.SplitRowCardinalityInconsistent "SplitRowCardinalityInconsistent",
    propertyStatsErrorShow Stats.SplitRowParseFailure "SplitRowParseFailure",

    getSplitRowExpectSuccess
        "Success"
        (defaultResponseBody [defaultSplit])
        defaultModel,

    getSplitRowsExpectSuccess
        "Success"
        (defaultResponseBody [defaultSplit { Stats.rows = [defaultRow, defaultRow] }])
        [defaultModel, defaultModel],

    getSplitRowExpectFailure
        "SplitRowCardinalityInconsistent"
        (defaultResponseBody [defaultSplit { Stats.rows = [take 3 defaultRow] }])
        (Stats.SplitRowCardinalityInconsistent $ show $ take 3 defaultRow),

    getSplitRowExpectFailure
        "SplitKeyNotFound (no rows)"
        (defaultResponseBody [defaultSplit { Stats.rows = [] }])
        (Stats.SplitKeyNotFound $ show defaultRowIdentifier),

    getSplitRowExpectFailure
        "SplitKeyNotFound (no key value)"
        (defaultResponseBody [defaultSplit { Stats.rows = [[]] }])
        (Stats.SplitKeyNotFound $ show defaultRowIdentifier),

    getSplitRowExpectFailure
        "SplitKeyNotFound (JSON parse error for key value)"
        (let rowWithBadValue = Aeson.Number 99 : defaultRow in defaultResponseBody [defaultSplit { Stats.rows = [rowWithBadValue] }])
        (Stats.SplitKeyNotFound $ show defaultRowIdentifier),

    getSplitRowExpectFailure
        "SplitNameNotFound"
        (defaultResponseBody [])
        (Stats.SplitNameNotFound $ Text.unpack defaultSplitName),

    getSplitRowExpectFailure
        "SplitColumnNameNotFound"
        (defaultResponseBody [defaultSplit { Stats.columns = [] }])
        (Stats.SplitColumnNameNotFound $ Text.unpack defaultColumnsKey),

    getSplitRowExpectFailure
        "SplitRowParseFailure (type mismatch)"
        (defaultResponseBody [defaultSplit { Stats.rows = [[Aeson.String defaultRowIdentifier, Aeson.String $ Text.pack $ show (a defaultModel), Aeson.String $ b defaultModel, Aeson.Number $ Sci.fromFloatDigits (c defaultModel)]] }])
        (Stats.SplitRowParseFailure "expected Integer, encountered String"),

    getSplitRowExpectFailure
        "SplitRowParseFailure (missing field)"
        (defaultResponseBody [defaultSplit { Stats.columns = fmap (\c -> if c == "B" then "!B" else c) defaultColumns }])
        (Stats.SplitRowParseFailure "key \"B\" not present"),

    getSplitRowExpectFailure
        "StatsResponseDecodeFailure (for Stats)"
        (Aeson.encode [defaultSplit])
        (Stats.StatsResponseDecodeFailure "Error in $: expected Stats, encountered Array"),

    HUnit.testCase
        "parseJSON invalid :: Parser Split -> Error"
        (case Aeson.fromJSON $ Aeson.String "foo" :: Aeson.Result Stats.Split of
            Aeson.Success _ -> HUnit.assertFailure "Parse should not have succeeded"
            Aeson.Error e -> e @?= "expected Split, encountered String")
    ]

propertyStatsErrorShow :: Show a => (String -> a) -> String -> Tasty.TestTree
propertyStatsErrorShow constructor exception =
    SC.testProperty testName property
    where
        testName = "show (" ++ exception ++ " message) == \"StatsError (" ++ exception ++ " message)\""
        property message = show (constructor message) == "StatsError (" ++ exception ++ " " ++ message ++ ")"

getSplitRowExpectFailure :: Tasty.TestName -> ByteString.ByteString -> Stats.StatsError -> Tasty.TestTree
getSplitRowExpectFailure testName responseBody expected = HUnit.testCase ("Get stat -> " <> testName) $ do
    eitherModel <- runStatsTest $ runAction statAction responseBody HTTP.ok200
    case eitherModel of
        Left err -> err @?= expected
        Right model -> HUnit.assertFailure $ show model

expectSuccess :: (Eq a, Show a) => MonadHttp.HttpT StatsTest a -> Tasty.TestName -> ByteString.ByteString -> a -> Tasty.TestTree
expectSuccess action testName responseBody expected =
    HUnit.testCase testName $ do
        eitherModel <- runStatsTest $ runAction action responseBody HTTP.ok200
        case eitherModel of
            Left err -> HUnit.assertFailure $ show err
            Right actual -> actual @?= expected

getSplitRowsExpectSuccess :: Tasty.TestName -> ByteString.ByteString -> [MockModel] -> Tasty.TestTree
getSplitRowsExpectSuccess name = expectSuccess (Stats.getSplitRowsGeneric "mockmodels" defaultSplitName defaultParams) $ "Get stats -> " <> name

getSplitRowExpectSuccess :: Tasty.TestName -> ByteString.ByteString -> MockModel -> Tasty.TestTree
getSplitRowExpectSuccess name = expectSuccess statAction $ "Get stat -> " <> name

statAction :: MonadHttp.HttpT StatsTest MockModel
statAction = Stats.getSplitRowGeneric "mockmodels" defaultSplitName defaultColumnsKey defaultRowIdentifier defaultParams

runAction :: MonadHttp.HttpT StatsTest a -> ByteString.ByteString -> HTTP.Status -> StatsTest a
runAction action responseBody responseStatus = MonadHttp.runHttpT
    action
    HTTP.Response {
        HTTP.responseStatus = responseStatus,
        HTTP.responseVersion = HTTP.http11,
        HTTP.responseHeaders = [],
        HTTP.responseBody = responseBody,
        HTTP.responseCookieJar = HTTP.createCookieJar [],
        HTTP.responseClose' = HTTP.ResponseClose (return () :: IO ())
    }

defaultParams :: Stats.StatsParameters
defaultParams = [("param", Just "value")]

defaultResponseBody :: [Stats.Split] -> ByteString.ByteString
defaultResponseBody splits = Aeson.encode defaultStats { Stats.splits = splits }

defaultStats :: Stats.Stats
defaultStats = Stats.Stats {
    splits = []
}

defaultSplit :: Stats.Split
defaultSplit = Stats.Split {
    name = defaultSplitName,
    columns = defaultColumns,
    rows = [defaultRow]
}

defaultSplitName :: Stats.SplitName
defaultSplitName = "name"

defaultColumnsKey :: Stats.SplitColumn
defaultColumnsKey = "key"

defaultRowIdentifier :: Text.Text
defaultRowIdentifier = "identifier"

defaultColumns :: [Stats.SplitColumn]
defaultColumns = [defaultColumnsKey, "A", "B", "C"]

defaultRow :: Stats.SplitRow
defaultRow = [Aeson.String defaultRowIdentifier, Aeson.Number $ Sci.scientific (a defaultModel) 0, Aeson.String $ b defaultModel, Aeson.Number $ Sci.fromFloatDigits (c defaultModel)]

defaultModel :: MockModel
defaultModel = MockModel { a = 1, b = "1", c = 1.1 }

type StatsTest = Except.ExceptT Stats.StatsError IO

runStatsTest :: StatsTest a -> IO (Either Stats.StatsError a)
runStatsTest = Except.runExceptT

data MockModel = MockModel {
    a :: Integer,
    b :: Text.Text,
    c :: Double
} deriving (Show, Eq)

instance Aeson.FromJSON MockModel where
    parseJSON (Aeson.Object o) = do
        a <- o .: "A"
        b <- o .: "B"
        c <- o .: "C"
        return MockModel {..}
    parseJSON invalid = Aeson.typeMismatch "MockModel" invalid
