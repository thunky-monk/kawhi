{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NBAStats.Tests where

import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.HTTP as MonadHTTP
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans as Trans
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Aeson ((.:))
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Scientific as Sci
import qualified Data.Text as Text
import qualified Network.HTTP.Client.Internal as HTTPInternal
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Types as HTTPTypes
import qualified NBAStats
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified Test.Tasty.SmallCheck as SC
import Test.Tasty.HUnit ((@?=))

tests :: Tasty.TestTree
tests = Tasty.testGroup "NBAStats.Resource" [
    propertyNBAStatsExceptionShow NBAStats.HTTPException "HTTPException",
    propertyNBAStatsExceptionShow NBAStats.PayloadDecodeError "PayloadDecodeError",
    propertyNBAStatsExceptionShow NBAStats.NoMatchingResult "NoMatchingResult",
    propertyNBAStatsExceptionShow NBAStats.NoMatchingRow "NoMatchingRow",
    propertyNBAStatsExceptionShow NBAStats.NoKeyInColumns "NoKeyInColumns",
    propertyNBAStatsExceptionShow NBAStats.NoValueForRowIndex "NoValueForRowIndex",
    propertyNBAStatsExceptionShow NBAStats.TableConversionError "TableConversionError",
    unitStat
        "NBAStats.stat -> Success"
        (defaultResponseBody [defaultResult])
        (\eitherModel -> case eitherModel of
            Left e -> HUnit.assertFailure $ show e
            Right model -> model @?= defaultModel),
    unitStatBase
        "NBAStats.stat -> HTTPException StatusCodeException"
        (Catch.catch
            (do
                _ <- unitStatRunAction unitStatAction (defaultResponseBody [defaultResult]) HTTPTypes.unauthorized401
                HUnit.assertFailure "StatusCodeException should have been thrown"
            )
            (\e@(NBAStats.HTTPException _) ->
                e @?= NBAStats.HTTPException (show $ HTTP.StatusCodeException HTTPTypes.unauthorized401 [] (HTTP.createCookieJar [])))),
    unitStat
        "NBAStats.stat -> NoValueForRowIndex"
        (defaultResponseBody [defaultResult { NBAStats.rows = [take 2 defaultRow] }])
        (assertEitherThrown
            (NBAStats.NoValueForRowIndex "2")
            "Should not have row value matching for index 2"),
    unitStat
        "NBAStats.stat -> NoMatchingRow (no rows)"
        (defaultResponseBody [defaultResult { NBAStats.rows = [] }])
        (assertEitherThrown
            (NBAStats.NoMatchingRow $ show defaultRowIdentifier)
            ("Should not have row with matching identifier: " ++ show defaultRowIdentifier)),
    unitStat
        "NBAStats.stat -> NoMatchingRow (no key value)"
        (defaultResponseBody [defaultResult { NBAStats.rows = [[]] }])
        (assertEitherThrown
            (NBAStats.NoMatchingRow $ show defaultRowIdentifier)
            ("Should not have row with matching identifier: " ++ show defaultRowIdentifier)),
    unitStat
        "NBAStats.stat -> NoMatchingRow (JSON parse error for key value)"
        (let rowWithBadValue = Aeson.Number 99 : defaultRow in defaultResponseBody [defaultResult { NBAStats.rows = [rowWithBadValue] }])
        (assertEitherThrown
            (NBAStats.NoMatchingRow $ show defaultRowIdentifier)
            ("Should not have row with matching identifier: " ++ show defaultRowIdentifier)),
    unitStat
        "NBAStats.stat -> NoMatchingResult"
        (defaultResponseBody [])
        (assertEitherThrown
            (NBAStats.NoMatchingResult $ Text.unpack defaultResultName)
            ("Should not have matching result:" ++ Text.unpack defaultResultName)),
    unitStat
        "NBAStats.stat -> NoKeyInColumns"
        (defaultResponseBody [defaultResult { NBAStats.columns = [] }])
        (assertEitherThrown
            (NBAStats.NoKeyInColumns $ Text.unpack defaultColumnsKey)
            ("Should not have key in columns: " ++ Text.unpack defaultColumnsKey)),
    unitStat
        "NBAStats.stat -> TableConversionError (type mismatch)"
        (defaultResponseBody [defaultResult { NBAStats.rows = [[Aeson.String defaultRowIdentifier, Aeson.String $ Text.pack $ show (a defaultModel), Aeson.String $ b defaultModel, Aeson.Number $ Sci.fromFloatDigits (c defaultModel)]] }])
        (assertEitherThrown (NBAStats.TableConversionError "failed to parse field A: expected Integral, encountered String")
        "Should not convert because of field is wrong type"),
    unitStat
        "NBAStats.stat -> TableConversionError (missing field)"
        (defaultResponseBody [defaultResult { NBAStats.columns = take (length defaultColumns - 1) defaultColumns }])
        (assertEitherThrown
            (NBAStats.TableConversionError "key \"C\" not present")
            "Should not convert because field is missing"),
    unitStat
        "NBAStats.stat -> PayloadDecodeError (for Resource)"
        (Aeson.encode [defaultResult])
        (assertEitherThrown
            (NBAStats.PayloadDecodeError "Error in $: expected Resource, encountered Array")
            "Should not be able to decode invalid JSON"),
    unitStats
        "NBAStats.stats -> Success"
        (defaultResponseBody [defaultResult { NBAStats.rows = [defaultRow, defaultRow] }])
        (\eitherModels -> case eitherModels of
            Left e -> HUnit.assertFailure $ show e
            Right models -> models @?= [defaultModel, defaultModel]),
    HUnit.testCase
        "parseJSON invalid :: Parser Result -> Error"
        (case Aeson.fromJSON $ Aeson.String "foo" :: Aeson.Result NBAStats.Result of
            Aeson.Success _ -> HUnit.assertFailure "Parse should not have succeeded"
            Aeson.Error e -> e @?= "expected Result, encountered String")
    ]

propertyNBAStatsExceptionShow :: Show a => (String -> a) -> String -> Tasty.TestTree
propertyNBAStatsExceptionShow constructor exception =
    SC.testProperty testName property
    where
        testName = "show (" ++ exception ++ " message) == \"NBAStatsException (" ++ exception ++ " message)\""
        property message = show (constructor message) == "NBAStatsException (" ++ exception ++ " " ++ message ++ ")"

assertEitherThrown :: Catch.Exception e => NBAStats.NBAStatsException -> String -> Either e MockModel -> IO ()
assertEitherThrown exception failureMessage eitherModel =
    case eitherModel of
        Left someE -> Catch.catch
            (Catch.throwM someE)
            (\(e :: NBAStats.NBAStatsException) -> e @?= exception)
        Right (_ :: MockModel) -> HUnit.assertFailure failureMessage

unitStat :: Tasty.TestName -> ByteString.ByteString -> (Either Catch.SomeException MockModel -> IO ()) -> Tasty.TestTree
unitStat = unitStatAssertEither unitStatAction

unitStats :: Tasty.TestName -> ByteString.ByteString -> (Either Catch.SomeException [MockModel] -> IO ()) -> Tasty.TestTree
unitStats = unitStatAssertEither $ NBAStats.stats "mockmodels" defaultResultName defaultParams

unitStatAction :: HTTP.Manager -> MockServer IO (Either Catch.SomeException MockModel)
unitStatAction = NBAStats.stat "mockmodels" defaultResultName defaultColumnsKey defaultRowIdentifier defaultParams

unitStatRunAction :: (HTTP.Manager -> MockServer IO a) -> ByteString.ByteString -> HTTPTypes.Status -> IO a
unitStatRunAction action responseBody responseStatus = do
    manager <- HTTP.newManager HTTP.tlsManagerSettings
    runMockServer
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

unitStatAssertEither :: (HTTP.Manager -> MockServer IO a) -> Tasty.TestName -> ByteString.ByteString -> (a -> IO ()) -> Tasty.TestTree
unitStatAssertEither function testName responseBody assert = unitStatBase testName $ do
    eitherModel <- unitStatRunAction function responseBody HTTPTypes.ok200
    assert eitherModel

defaultParams :: NBAStats.Parameters
defaultParams = [("param", Just "value")]

defaultResponseBody :: [NBAStats.Result] -> ByteString.ByteString
defaultResponseBody results = Aeson.encode defaultResource { NBAStats.results = results }

defaultResource :: NBAStats.Resource
defaultResource = NBAStats.Resource {
    results = []
}

defaultResult :: NBAStats.Result
defaultResult = NBAStats.Result {
    name = defaultResultName,
    columns = defaultColumns,
    rows = [defaultRow]
}

defaultResultName :: NBAStats.ResultName
defaultResultName = "name"

defaultColumnsKey :: NBAStats.Column
defaultColumnsKey = "key"

defaultRowIdentifier :: Text.Text
defaultRowIdentifier = "identifier"

defaultColumns :: [NBAStats.Column]
defaultColumns = [defaultColumnsKey, "A", "B", "C"]

defaultRow :: NBAStats.Row
defaultRow = [Aeson.String defaultRowIdentifier, Aeson.Number $ Sci.scientific (a defaultModel) 0, Aeson.String $ b defaultModel, Aeson.Number $ Sci.fromFloatDigits (c defaultModel)]

defaultModel :: MockModel
defaultModel = MockModel { a = 1, b = "1", c = 1.1 }

newtype MockServer m a = MockServer { mockServer :: Reader.ReaderT (HTTP.Response ByteString.ByteString) m a }
    deriving (Applicative, Functor, Monad, Trans.MonadTrans, Reader.MonadReader (HTTP.Response ByteString.ByteString), Catch.MonadThrow, Catch.MonadCatch, Trans.MonadIO)

runMockServer :: MockServer m a -> HTTP.Response ByteString.ByteString -> m a
runMockServer (MockServer s) = Reader.runReaderT s

instance Catch.MonadThrow m => MonadHTTP.MonadHTTP (MockServer m) where
    request _ _ = check
        where
            check = do
                response <- Reader.ask
                let status = HTTP.responseStatus response
                if status >= HTTPTypes.ok200 && status < HTTPTypes.multipleChoices300
                    then return response
                    else Catch.throwM $ HTTP.StatusCodeException status [] (HTTP.createCookieJar [])

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
