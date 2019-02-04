module Stetson.Rest where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Cowboy.Req (Req)
import Erl.Data.List (List)
import Erl.Data.Tuple (Tuple2)
import Stetson (AcceptHandler, HttpMethod, InitHandler, InitResult(..), ProvideHandler, RestHandler, RestResult(..), StetsonHandler(..))

handler :: forall state. InitHandler state -> RestHandler state
handler init = {
  init
  , allowedMethods: Nothing
  , resourceExists: Nothing
  , contentTypesAccepted: Nothing
  , contentTypesProvided: Nothing
  }

allowedMethods :: forall state. (Req -> state -> Effect (RestResult (List HttpMethod) state)) -> RestHandler state -> RestHandler state
allowedMethods fn handler = (handler { allowedMethods = Just fn })

resourceExists :: forall state. (Req -> state -> Effect (RestResult Boolean state)) -> RestHandler state -> RestHandler state
resourceExists fn handler = (handler { resourceExists = Just fn })

contentTypesAccepted :: forall state. (Req -> state -> Effect (RestResult (List (Tuple2 String (AcceptHandler state))) state)) -> RestHandler state -> RestHandler state
contentTypesAccepted fn handler = (handler { contentTypesAccepted = Just fn  })

contentTypesProvided :: forall state. (Req -> state -> Effect (RestResult (List (Tuple2 String (ProvideHandler state))) state)) -> RestHandler state -> RestHandler state
contentTypesProvided fn handler = (handler { contentTypesProvided = Just fn  })

initResult :: forall state. Req -> state -> Effect (InitResult state)
initResult rq st = pure $ InitOk rq st

result :: forall reply state. reply -> Req -> state -> Effect (RestResult reply state)
result re rq st = pure $ RestOk re rq st

yeeha :: forall state. RestHandler state -> StetsonHandler state
yeeha = Rest