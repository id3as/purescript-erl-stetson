module Stetson.Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Data.Tuple (snd)
import Pinto.Types (StartLinkResult(..))
import Stetson.Test.Handlers (startLink, stopLink)
import Stetson.Test.Requests as Requests
import Test.Assert (assertEqual)

foreign import startup :: Effect Unit

type State
  = Unit

main :: Effect Unit
main = do
  startup
  legacyResult <- testLegacySyntaxGet
  assertEqual { actual: legacyResult, expected: Just "{\"handler\":\"fullyLoadedHandler\"}" }
  currentResult <- testCurrentSyntaxGet
  assertEqual { actual: currentResult, expected: Just "{\"handler\":\"fullyLoadedHandler\"}" }
  pure $ unit

testLegacySyntaxGet :: Effect (Maybe String)
testLegacySyntaxGet = do
  startLinkResult <- startLink true
  result <- case startLinkResult of
    Right _ -> do
      result <- Requests.testFullyLoaded 3000
      pure $ snd result
    _ -> pure $ Nothing
  void $ stopLink
  pure $ result

testCurrentSyntaxGet :: Effect (Maybe String)
testCurrentSyntaxGet = do
  startLinkResult <- startLink false
  result <- case startLinkResult of
    Right _ -> do
      result <- Requests.testFullyLoaded 3001
      pure $ snd result
    _ -> pure $ Nothing
  void $ stopLink
  pure $ result
