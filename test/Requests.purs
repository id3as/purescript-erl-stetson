module Stetson.Test.Requests where

import Data.Maybe (Maybe)
import Effect (Effect)
import Erl.Atom (Atom)
import Erl.Data.Tuple (Tuple2)

foreign import testFullyLoaded :: String -> Int -> Effect (Tuple2 Atom (Maybe String))
