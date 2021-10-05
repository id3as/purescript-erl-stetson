module Test.Main where

import Prelude

import Control.Monad.Free (Free)
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Erl.Data.Tuple (snd)
import Erl.Test.EUnit (TestF, runTests, setup, test)
import Stetson.Test.Handlers (ServerConfig(..), startLink, stopLink)
import Stetson.Test.Requests as Requests
import Test.Assert (assertEqual)

foreign import startup :: Effect Unit

main :: Effect Unit
main = void $ runTests tests

fullyLoadedUrl :: String
fullyLoadedUrl = "/api/testfullyloaded"
  
nestedUrl :: String
nestedUrl = "/api/one/api/testfullyloaded" -- I know

tests :: Free TestF Unit
tests =
  setup startup do
    test "legacy syntax get" do
      void $ startLink OldStyle >>= either (const $ throw "startlink failed") pure
      result <- snd <$> Requests.testFullyLoaded fullyLoadedUrl 3000
      stopLink
      assertEqual { actual: result, expected: Just "{\"handler\":\"fullyLoadedHandler\"}" }
    test "current syntax get" do
      void $ startLink NewStyle >>= either (const $ throw "startlink failed") pure
      result <- snd <$> Requests.testFullyLoaded fullyLoadedUrl 3001
      stopLink
      assertEqual { actual: result, expected: Just "{\"handler\":\"fullyLoadedHandler\"}" }
    test "nested" do
      void $ startLink NestedRoutes >>= either (const $ throw "startlink failed") pure
      result <- snd <$> Requests.testFullyLoaded nestedUrl 3002
      stopLink
      assertEqual { actual: result, expected: Just "{\"handler\":\"fullyLoadedHandler\"}" }
