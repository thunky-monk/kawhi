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
    unitStat
        "Stats.stat -> Success"
        (defaultResponseBody [defaultResult])
        (\eitherModel -> case eitherModel of
            Left e -> HUnit.assertFailure $ show e
            Right model -> model @?= defaultModel),
    unitStatBase
        "Stats.stat -> HTTPException StatusCodeException"
        (Catch.catch
            (do
                _ <- unitStatRunAction unitStatAction (defaultResponseBody [defaultResult]) HTTPTypes.unauthorized401
                HUnit.assertFailure "StatusCodeException should have been thrown"
            )
            (\e@(Stats.HTTPException _) ->
                e @?= Stats.HTTPException (show $ HTTP.StatusCodeException HTTPTypes.unauthorized401 [] (HTTP.createCookieJar [])))),
    unitStat
        "Stats.stat -> NoValueForRowIndex"
        (defaultResponseBody [defaultResult { Stats.rows = [take 2 defaultRow] }])
        (assertEitherThrown
            (Stats.NoValueForRowIndex "2")
            "Should not have row value matching for index 2"),
    unitStat
        "Stats.stat -> NoMatchingRow (no rows)"
        (defaultResponseBody [defaultResult { Stats.rows = [] }])
        (assertEitherThrown
            (Stats.NoMatchingRow $ show defaultRowIdentifier)
            ("Should not have row with matching identifier: " ++ show defaultRowIdentifier)),
    unitStat
        "Stats.stat -> NoMatchingRow (no key value)"
        (defaultResponseBody [defaultResult { Stats.rows = [[]] }])
        (assertEitherThrown
            (Stats.NoMatchingRow $ show defaultRowIdentifier)
            ("Should not have row with matching identifier: " ++ show defaultRowIdentifier)),
    unitStat
        "Stats.stat -> NoMatchingRow (JSON parse error for key value)"
        (let rowWithBadValue = Aeson.Number 99 : defaultRow in defaultResponseBody [defaultResult { Stats.rows = [rowWithBadValue] }])
        (assertEitherThrown
            (Stats.NoMatchingRow $ show defaultRowIdentifier)
            ("Should not have row with matching identifier: " ++ show defaultRowIdentifier)),
    unitStat
        "Stats.stat -> NoMatchingResult"
        (defaultResponseBody [])
        (assertEitherThrown
            (Stats.NoMatchingResult $ Text.unpack defaultResultName)
            ("Should not have matching result:" ++ Text.unpack defaultResultName)),
    unitStat
        "Stats.stat -> NoKeyInColumns"
        (defaultResponseBody [defaultResult { Stats.columns = [] }])
        (assertEitherThrown
            (Stats.NoKeyInColumns $ Text.unpack defaultColumnsKey)
            ("Should not have key in columns: " ++ Text.unpack defaultColumnsKey)),
    unitStat
        "Stats.stat -> TableConversionError (type mismatch)"
        (defaultResponseBody [defaultResult { Stats.rows = [[Aeson.String defaultRowIdentifier, Aeson.String $ Text.pack $ show (a defaultModel), Aeson.String $ b defaultModel, Aeson.Number $ Sci.fromFloatDigits (c defaultModel)]] }])
        (assertEitherThrown (Stats.TableConversionError "failed to parse field A: expected Integral, encountered String")
        "Should not convert because of field is wrong type"),
    unitStat
        "Stats.stat -> TableConversionError (missing field)"
        (defaultResponseBody [defaultResult { Stats.columns = take (length defaultColumns - 1) defaultColumns }])
        (assertEitherThrown
            (Stats.TableConversionError "key \"C\" not present")
            "Should not convert because field is missing"),
    unitStat
        "Stats.stat -> PayloadDecodeError (for Resource)"
        (Aeson.encode [defaultResult])
        (assertEitherThrown
            (Stats.PayloadDecodeError "Error in $: expected Resource, encountered Array")
            "Should not be able to decode invalid JSON"),
    unitStats
        "Stats.stats -> Success"
        (defaultResponseBody [defaultResult { Stats.rows = [defaultRow, defaultRow] }])
        (\eitherModels -> case eitherModels of
            Left e -> HUnit.assertFailure $ show e
            Right models -> models @?= [defaultModel, defaultModel]),
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

assertEitherThrown :: Catch.Exception e => Stats.StatsException -> String -> Either e MockModel -> IO ()
assertEitherThrown exception failureMessage eitherModel =
    case eitherModel of
        Left someE -> Catch.catch
            (Catch.throwM someE)
            (\(e :: Stats.StatsException) -> e @?= exception)
        Right (_ :: MockModel) -> HUnit.assertFailure failureMessage

unitStat :: Tasty.TestName -> ByteString.ByteString -> (Either Catch.SomeException MockModel -> IO ()) -> Tasty.TestTree
unitStat = unitStatAssertEither unitStatAction

unitStats :: Tasty.TestName -> ByteString.ByteString -> (Either Catch.SomeException [MockModel] -> IO ()) -> Tasty.TestTree
unitStats = unitStatAssertEither $ Stats.stats "mockmodels" defaultResultName defaultParams

unitStatAction :: HTTP.Manager -> MonadHTTP.MockHTTP IO (Either Catch.SomeException MockModel)
unitStatAction = Stats.stat "mockmodels" defaultResultName defaultColumnsKey defaultRowIdentifier defaultParams

unitStatRunAction :: (HTTP.Manager -> MonadHTTP.MockHTTP IO a) -> ByteString.ByteString -> HTTPTypes.Status -> IO a
unitStatRunAction action responseBody responseStatus = do
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

unitStatBase :: Tasty.TestName
                       -> HUnit.Assertion -> Tasty.TestTree
unitStatBase = HUnit.testCase

unitStatAssertEither :: (HTTP.Manager -> MonadHTTP.MockHTTP IO a) -> Tasty.TestName -> ByteString.ByteString -> (a -> IO ()) -> Tasty.TestTree
unitStatAssertEither function testName responseBody assert = unitStatBase testName $ do
    eitherModel <- unitStatRunAction function responseBody HTTPTypes.ok200
    assert eitherModel

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
