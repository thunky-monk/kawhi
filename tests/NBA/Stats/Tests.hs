{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NBA.Stats.Tests where

import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.HTTP as MonadHTTP
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Aeson ((.:))
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as ByteString
import Data.Monoid ((<>))
import qualified Data.Scientific as Sci
import qualified Data.Text as Text
import qualified Network.HTTP.Client.Internal as HTTPInternal
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types as HTTPTypes
import qualified NBA.Stats as Stats
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit ((@?=))

tests :: Tasty.TestTree
tests = Tasty.testGroup "NBA.Stats" [
    SC.testProperty "getRequest == HTTP.parseUrl" $
        \path -> SC.monadic $ do
            request <- Stats.getRequest $ Char8.pack path
            model <- HTTP.parseUrl $ "http://stats.nba.com/stats/" <> path
            return $ show request == show model,
    propertyStatsExceptionShow Stats.HTTPException "HTTPException",
    propertyStatsExceptionShow Stats.PayloadDecodeError "PayloadDecodeError",
    propertyStatsExceptionShow Stats.NoMatchingResult "NoMatchingResult",
    propertyStatsExceptionShow Stats.NoMatchingRow "NoMatchingRow",
    propertyStatsExceptionShow Stats.NoKeyInColumns "NoKeyInColumns",
    propertyStatsExceptionShow Stats.NoValueForRowIndex "NoValueForRowIndex",
    propertyStatsExceptionShow Stats.TableConversionError "TableConversionError",

    getStatExpectSuccess
        "Success"
        (defaultResponseBody [defaultResult])
        defaultModel,

    getStatsExpectSuccess
        "Success"
        (defaultResponseBody [defaultResult { Stats.rows = [defaultRow, defaultRow] }])
        [defaultModel, defaultModel],

    HUnit.testCase
        "Get stat -> HTTPException StatusCodeException"
        (Catch.catch
            (do
                _ <- runAction statAction (defaultResponseBody [defaultResult]) HTTPTypes.unauthorized401
                HUnit.assertFailure "StatusCodeException should have been thrown"
            )
            (\(e :: Stats.StatsException) ->
                e @?= Stats.HTTPException (show $ HTTP.StatusCodeException HTTPTypes.unauthorized401 [] (HTTP.createCookieJar [])))),

    getStatExpectFailure
        "NoValueForRowIndex"
        (defaultResponseBody [defaultResult { Stats.rows = [take 2 defaultRow] }])
        (Stats.NoValueForRowIndex "2")
        "Should not have row value matching for index 2",

    getStatExpectFailure
        "NoValueForRowIndex"
        (defaultResponseBody [defaultResult { Stats.rows = [take 2 defaultRow] }])
        (Stats.NoValueForRowIndex "2")
        "Should not have row value matching for index 2",

    getStatExpectFailure
        "NoMatchingRow (no rows)"
        (defaultResponseBody [defaultResult { Stats.rows = [] }])
        (Stats.NoMatchingRow $ show defaultRowIdentifier)
        ("Should not have row with matching identifier: " ++ show defaultRowIdentifier),

    getStatExpectFailure
        "NoMatchingRow (no key value)"
        (defaultResponseBody [defaultResult { Stats.rows = [[]] }])
        (Stats.NoMatchingRow $ show defaultRowIdentifier)
        ("Should not have row with matching identifier: " ++ show defaultRowIdentifier),

    getStatExpectFailure
        "NoMatchingRow (JSON parse error for key value)"
        (let rowWithBadValue = Aeson.Number 99 : defaultRow in defaultResponseBody [defaultResult { Stats.rows = [rowWithBadValue] }])
        (Stats.NoMatchingRow $ show defaultRowIdentifier)
        ("Should not have row with matching identifier: " ++ show defaultRowIdentifier),

    getStatExpectFailure
        "NoMatchingResult"
        (defaultResponseBody [])
        (Stats.NoMatchingResult $ Text.unpack defaultResultName)
        ("Should not have matching result:" ++ Text.unpack defaultResultName),

    getStatExpectFailure
        "NoKeyInColumns"
        (defaultResponseBody [defaultResult { Stats.columns = [] }])
        (Stats.NoKeyInColumns $ Text.unpack defaultColumnsKey)
        ("Should not have key in columns: " ++ Text.unpack defaultColumnsKey),

    getStatExpectFailure
        "TableConversionError (type mismatch)"
        (defaultResponseBody [defaultResult { Stats.rows = [[Aeson.String defaultRowIdentifier, Aeson.String $ Text.pack $ show (a defaultModel), Aeson.String $ b defaultModel, Aeson.Number $ Sci.fromFloatDigits (c defaultModel)]] }])
        (Stats.TableConversionError "failed to parse field A: expected Integral, encountered String")
        "Should not convert because of field is wrong type",

    getStatExpectFailure
        "TableConversionError (missing field)"
        (defaultResponseBody [defaultResult { Stats.columns = take (length defaultColumns - 1) defaultColumns }])
        (Stats.TableConversionError "key \"C\" not present")
        "Should not convert because field is missing",

    getStatExpectFailure
        "PayloadDecodeError (for Resource)"
        (Aeson.encode [defaultResult])
        (Stats.PayloadDecodeError "Error in $: expected Resource, encountered Array")
        "Should not be able to decode invalid JSON",

    HUnit.testCase
        "parseJSON invalid :: Parser Result -> Error"
        (case Aeson.fromJSON $ Aeson.String "foo" :: Aeson.Result Stats.Result of
            Aeson.Success _ -> HUnit.assertFailure "Parse should not have succeeded"
            Aeson.Error e -> e @?= "expected Result, encountered String")
    ]

propertyStatsExceptionShow :: Show a => (String -> a) -> String -> Tasty.TestTree
propertyStatsExceptionShow constructor exception =
    SC.testProperty testName property
    where
        testName = "show (" ++ exception ++ " message) == \"StatsException (" ++ exception ++ " message)\""
        property message = show (constructor message) == "StatsException (" ++ exception ++ " " ++ message ++ ")"

getStatExpectFailure :: Tasty.TestName -> ByteString.ByteString -> Stats.StatsException -> String -> Tasty.TestTree
getStatExpectFailure testName responseBody expectedException failureMessage =
    HUnit.testCase ("Get stat -> " <> testName) $ Catch.catch
        (runAction statAction responseBody HTTPTypes.ok200 >> HUnit.assertFailure failureMessage)
        (\(actualException :: Stats.StatsException) -> actualException @?= expectedException)

expectSuccess :: (Eq a, Show a) => (HTTP.Manager -> MonadHTTP.MockHTTP IO a) -> Tasty.TestName -> ByteString.ByteString -> a -> Tasty.TestTree
expectSuccess action testName responseBody expected =
    HUnit.testCase testName $ Catch.catchAll
        (runAction action responseBody HTTPTypes.ok200 >>= (@?= expected))
        (HUnit.assertFailure . Catch.displayException)

getStatsExpectSuccess :: Tasty.TestName -> ByteString.ByteString -> [MockModel] -> Tasty.TestTree
getStatsExpectSuccess name = expectSuccess (Stats.getStats "mockmodels" defaultResultName defaultParams) $ "Get stats -> " <> name

getStatExpectSuccess :: Tasty.TestName -> ByteString.ByteString -> MockModel -> Tasty.TestTree
getStatExpectSuccess name = expectSuccess statAction $ "Get stat -> " <> name

statAction :: HTTP.Manager -> MonadHTTP.MockHTTP IO MockModel
statAction = Stats.getStat "mockmodels" defaultResultName defaultColumnsKey defaultRowIdentifier defaultParams

runAction :: (HTTP.Manager -> MonadHTTP.MockHTTP IO a) -> ByteString.ByteString -> HTTPTypes.Status -> IO a
runAction action responseBody responseStatus = do
    manager <- HTTP.newManager HTTP.tlsManagerSettings
    MonadHTTP.runMockHTTP
        (action manager)
        HTTPInternal.Response {
            HTTP.responseStatus = responseStatus,
            HTTP.responseVersion = HTTPTypes.http11,
            HTTP.responseHeaders = [],
            HTTP.responseBody = responseBody,
            HTTP.responseCookieJar = HTTP.createCookieJar [],
            HTTPInternal.responseClose' = HTTPInternal.ResponseClose (return () :: IO ())
        }

defaultParams :: Stats.Parameters
defaultParams = [("param", Just "value")]

defaultResponseBody :: [Stats.Result] -> ByteString.ByteString
defaultResponseBody results = Aeson.encode defaultResource { Stats.results = results }

defaultResource :: Stats.Resource
defaultResource = Stats.Resource {
    results = []
}

defaultResult :: Stats.Result
defaultResult = Stats.Result {
    name = defaultResultName,
    columns = defaultColumns,
    rows = [defaultRow]
}

defaultResultName :: Stats.ResultName
defaultResultName = "name"

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
