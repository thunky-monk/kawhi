{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NBA.Stats.Tests where

import qualified Control.Monad.Except as Except
import qualified Control.Monad.HTTP as MonadHTTP
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Aeson ((.:))
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as ByteString
import Data.Monoid ((<>))
import qualified Data.Scientific as Sci
import qualified Data.Text as Text
import qualified Network.HTTP.Client.Internal as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified NBA.Stats as Stats
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit ((@?=))

tests :: Tasty.TestTree
tests = Tasty.testGroup "NBA.Stats" [
    SC.testProperty "getRequest == HTTP.parseUrl" $
        \path -> SC.monadic $ do
            let request = Stats.getRequest $ Char8.pack path
            model <- HTTP.parseUrl $ "http://stats.nba.com/stats/" <> path
            return $ show request == show model,
    propertyStatsErrorShow Stats.PayloadDecodeError "PayloadDecodeFailure",
    propertyStatsErrorShow Stats.NoMatchingSplit "NoMatchingSplit",
    propertyStatsErrorShow Stats.NoMatchingRow "NoMatchingRow",
    propertyStatsErrorShow Stats.NoKeyInColumns "NoKeyInColumns",
    propertyStatsErrorShow Stats.NoValueForRowIndex "NoValueForRowIndex",
    propertyStatsErrorShow Stats.TableConversionError "TableConversionFailure",

    getSplitRowExpectSuccess
        "Success"
        (defaultResponseBody [defaultSplit])
        defaultModel,

    getSplitRowsExpectSuccess
        "Success"
        (defaultResponseBody [defaultSplit { Stats.rows = [defaultRow, defaultRow] }])
        [defaultModel, defaultModel],

    getSplitRowExpectFailure
        "NoValueForRowIndex"
        (defaultResponseBody [defaultSplit { Stats.rows = [take 2 defaultRow] }])
        (Stats.NoValueForRowIndex "2"),

    getSplitRowExpectFailure
        "NoValueForRowIndex"
        (defaultResponseBody [defaultSplit { Stats.rows = [take 2 defaultRow] }])
        (Stats.NoValueForRowIndex "2"),

    getSplitRowExpectFailure
        "NoMatchingRow (no rows)"
        (defaultResponseBody [defaultSplit { Stats.rows = [] }])
        (Stats.NoMatchingRow $ show defaultRowIdentifier),

    getSplitRowExpectFailure
        "NoMatchingRow (no key value)"
        (defaultResponseBody [defaultSplit { Stats.rows = [[]] }])
        (Stats.NoMatchingRow $ show defaultRowIdentifier),

    getSplitRowExpectFailure
        "NoMatchingRow (JSON parse error for key value)"
        (let rowWithBadValue = Aeson.Number 99 : defaultRow in defaultResponseBody [defaultSplit { Stats.rows = [rowWithBadValue] }])
        (Stats.NoMatchingRow $ show defaultRowIdentifier),

    getSplitRowExpectFailure
        "NoMatchingSplit"
        (defaultResponseBody [])
        (Stats.NoMatchingSplit $ Text.unpack defaultSplitName),

    getSplitRowExpectFailure
        "NoKeyInColumns"
        (defaultResponseBody [defaultSplit { Stats.columns = [] }])
        (Stats.NoKeyInColumns $ Text.unpack defaultColumnsKey),

    getSplitRowExpectFailure
        "TableConversionError (type mismatch)"
        (defaultResponseBody [defaultSplit { Stats.rows = [[Aeson.String defaultRowIdentifier, Aeson.String $ Text.pack $ show (a defaultModel), Aeson.String $ b defaultModel, Aeson.Number $ Sci.fromFloatDigits (c defaultModel)]] }])
        (Stats.TableConversionError "failed to parse field A: expected Integral, encountered String"),

    getSplitRowExpectFailure
        "TableConversionError (missing field)"
        (defaultResponseBody [defaultSplit { Stats.columns = take (length defaultColumns - 1) defaultColumns }])
        (Stats.TableConversionError "key \"C\" not present"),

    getSplitRowExpectFailure
        "PayloadDecodeError (for Stats)"
        (Aeson.encode [defaultSplit])
        (Stats.PayloadDecodeError "Error in $: expected Stats, encountered Array"),

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

expectSuccess :: (Eq a, Show a) => MonadHTTP.MockHTTP StatsTest a -> Tasty.TestName -> ByteString.ByteString -> a -> Tasty.TestTree
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

statAction :: MonadHTTP.MockHTTP StatsTest MockModel
statAction = Stats.getSplitRowGeneric "mockmodels" defaultSplitName defaultColumnsKey defaultRowIdentifier defaultParams

runAction :: MonadHTTP.MockHTTP StatsTest a -> ByteString.ByteString -> HTTP.Status -> StatsTest a
runAction action responseBody responseStatus = MonadHTTP.runMockHTTP
    action
    HTTP.Response {
        HTTP.responseStatus = responseStatus,
        HTTP.responseVersion = HTTP.http11,
        HTTP.responseHeaders = [],
        HTTP.responseBody = responseBody,
        HTTP.responseCookieJar = HTTP.createCookieJar [],
        HTTP.responseClose' = HTTP.ResponseClose (return () :: IO ())
    }

defaultParams :: Stats.Parameters
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

defaultColumnsKey :: Stats.Column
defaultColumnsKey = "key"

defaultRowIdentifier :: Text.Text
defaultRowIdentifier = "identifier"

defaultColumns :: [Stats.Column]
defaultColumns = [defaultColumnsKey, "A", "B", "C"]

defaultRow :: Stats.Row
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
