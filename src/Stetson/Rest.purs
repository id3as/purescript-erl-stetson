module Stetson.Rest where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Cowboy.Handlers.Rest (MovedResult)
import Erl.Cowboy.Req (Req)
import Erl.Data.List (List)
import Erl.Data.Tuple (Tuple2)
import Stetson (AcceptHandler, Authorized, HttpMethod, InitHandler, InitResult(..), ProvideHandler, RestHandler, RestResult(..), StetsonHandler(..))

handler :: forall state. InitHandler state -> RestHandler state
handler init = {
  init
  , allowedMethods: Nothing
  , resourceExists: Nothing
  , contentTypesAccepted: Nothing
  , contentTypesProvided: Nothing
  , deleteResource: Nothing
  , isAuthorized: Nothing
  , movedTemporarily: Nothing
  , movedPermanently: Nothing
  , serviceAvailable: Nothing
  , previouslyExisted: Nothing
  , forbidden: Nothing
  }

allowedMethods :: forall state. (Req -> state -> Effect (RestResult (List HttpMethod) state)) -> RestHandler state -> RestHandler state
allowedMethods fn handler = (handler { allowedMethods = Just fn })

resourceExists :: forall state. (Req -> state -> Effect (RestResult Boolean state)) -> RestHandler state -> RestHandler state
resourceExists fn handler = (handler { resourceExists = Just fn })

isAuthorized :: forall state. (Req -> state -> Effect (RestResult Authorized state)) -> RestHandler state -> RestHandler state
isAuthorized fn handler = (handler { isAuthorized = Just fn })

contentTypesAccepted :: forall state. (Req -> state -> Effect (RestResult (List (Tuple2 String (AcceptHandler state))) state)) -> RestHandler state -> RestHandler state
contentTypesAccepted fn handler = (handler { contentTypesAccepted = Just fn  })

contentTypesProvided :: forall state. (Req -> state -> Effect (RestResult (List (Tuple2 String (ProvideHandler state))) state)) -> RestHandler state -> RestHandler state
contentTypesProvided fn handler = (handler { contentTypesProvided = Just fn  })

deleteResource :: forall state. (Req -> state -> Effect (RestResult Boolean state)) -> RestHandler state -> RestHandler state
deleteResource fn handler = (handler { deleteResource = Just fn })

movedTemporarily :: forall state. (Req -> state -> Effect (RestResult MovedResult state)) -> RestHandler state -> RestHandler state
movedTemporarily fn handler = (handler { movedTemporarily = Just fn })

movedPermanently :: forall state. (Req -> state -> Effect (RestResult MovedResult state)) -> RestHandler state -> RestHandler state
movedPermanently fn handler = (handler { movedPermanently = Just fn })

serviceAvailable :: forall state. (Req -> state -> Effect (RestResult Boolean state)) -> RestHandler state -> RestHandler state
serviceAvailable fn handler = (handler { serviceAvailable = Just fn })

previouslyExisted :: forall state. (Req -> state -> Effect (RestResult Boolean state)) -> RestHandler state -> RestHandler state
previouslyExisted fn handler = (handler { previouslyExisted = Just fn })

forbidden :: forall state. (Req -> state -> Effect (RestResult Boolean state)) -> RestHandler state -> RestHandler state
forbidden fn handler = (handler { forbidden = Just fn })

initResult :: forall state. Req -> state -> Effect (InitResult state)
initResult rq st = pure $ InitOk rq st

result :: forall reply state. reply -> Req -> state -> Effect (RestResult reply state)
result re rq st = pure $ RestOk re rq st

yeeha :: forall state. RestHandler state -> StetsonHandler state
yeeha = Rest
