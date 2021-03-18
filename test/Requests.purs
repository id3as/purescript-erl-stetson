module Test.Requests where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (Atom(..))
import Erl.Data.Tuple (Tuple2(..))

foreign import test_fully_loaded :: Int -> Effect (Tuple2 Atom (Maybe String))

testFullyLoaded :: Int -> Effect (Tuple2 Atom (Maybe String))
testFullyLoaded port = do test_fully_loaded port
