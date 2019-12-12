-- | This module contains the functions necessary to define a rest handler for a route in Stetson/Cowboy
-- | This maps pretty much 1-1 onto https://ninenines.eu/docs/en/cowboy/2.5/guide/rest_handlers/#_callbacks
-- | Although only the handlers that we have needed so far are defined - feel free to send pull requests that add the ones you need
module Stetson.Rest ( handler
                    , allowedMethods
                    , resourceExists
                    , isAuthorized
                    , isConflict
                    , contentTypesAccepted
                    , contentTypesProvided
                    , deleteResource
                    , movedTemporarily
                    , movedPermanently
                    , serviceAvailable
                    , previouslyExisted
                    , forbidden
                    , initResult
                    , result
                    , yeeha
                    )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Cowboy.Handlers.Rest (MovedResult)
import Erl.Cowboy.Req (Req)
import Erl.Data.List (List)
import Erl.Data.Tuple (Tuple2)
import Stetson (AcceptHandler, Authorized, HttpMethod, InitHandler, InitResult(..), ProvideHandler, RestHandler, RestResult(..), StetsonHandler(..))

-- | Create a cowboy REST handler with the provided Init handler and no callbacks defined
handler :: forall state. InitHandler state -> RestHandler state
handler init = {
  init
  , allowedMethods: Nothing
  , resourceExists: Nothing
  , contentTypesAccepted: Nothing
  , contentTypesProvided: Nothing
  , deleteResource: Nothing
  , isAuthorized: Nothing
  , isConflict: Nothing
  , movedTemporarily: Nothing
  , movedPermanently: Nothing
  , serviceAvailable: Nothing
  , previouslyExisted: Nothing
  , forbidden: Nothing
  }

-- | Add an allowedMethods callback to the provided RestHandler
allowedMethods :: forall state. (Req -> state -> Effect (RestResult (List HttpMethod) state)) -> RestHandler state -> RestHandler state
allowedMethods fn handler_ = (handler_ { allowedMethods = Just fn })

-- | Add a resourceExists callback to the provided RestHandler
resourceExists :: forall state. (Req -> state -> Effect (RestResult Boolean state)) -> RestHandler state -> RestHandler state
resourceExists fn handler_ = (handler_ { resourceExists = Just fn })

-- | Add an isAuthorized callback to the provided RestHandler
isAuthorized :: forall state. (Req -> state -> Effect (RestResult Authorized state)) -> RestHandler state -> RestHandler state
isAuthorized fn handler_ = (handler_ { isAuthorized = Just fn })


-- | Add an isConflict callback to the provided RestHandler
isConflict :: forall state. (Req -> state -> Effect (RestResult Boolean state)) -> RestHandler state -> RestHandler state
isConflict fn handler_ = (handler_ { isConflict = Just fn })

-- | Add a contentTypesAccepted callback to the provided RestHandler
contentTypesAccepted :: forall state. (Req -> state -> Effect (RestResult (List (Tuple2 String (AcceptHandler state))) state)) -> RestHandler state -> RestHandler state
contentTypesAccepted fn handler_ = (handler_ { contentTypesAccepted = Just fn  })

-- | Add a contentTypesProvided callback to the provided RestHandler
contentTypesProvided :: forall state. (Req -> state -> Effect (RestResult (List (Tuple2 String (ProvideHandler state))) state)) -> RestHandler state -> RestHandler state
contentTypesProvided fn handler_ = (handler_ { contentTypesProvided = Just fn  })

-- | Add a deleteResource callback to the provided RestHandler
deleteResource :: forall state. (Req -> state -> Effect (RestResult Boolean state)) -> RestHandler state -> RestHandler state
deleteResource fn handler_ = (handler_ { deleteResource = Just fn })

-- | Add a movedTemporarily callback to the provided RestHandler
movedTemporarily :: forall state. (Req -> state -> Effect (RestResult MovedResult state)) -> RestHandler state -> RestHandler state
movedTemporarily fn handler_ = (handler_ { movedTemporarily = Just fn })

-- | Add a movedPermanently callback to the provided RestHandler
movedPermanently :: forall state. (Req -> state -> Effect (RestResult MovedResult state)) -> RestHandler state -> RestHandler state
movedPermanently fn handler_ = (handler_ { movedPermanently = Just fn })

-- | Add a serviceAvailable callback to the provided RestHandler
serviceAvailable :: forall state. (Req -> state -> Effect (RestResult Boolean state)) -> RestHandler state -> RestHandler state
serviceAvailable fn handler_ = (handler_ { serviceAvailable = Just fn })

-- | Add a previouslyExisted callback to the provided RestHandler
previouslyExisted :: forall state. (Req -> state -> Effect (RestResult Boolean state)) -> RestHandler state -> RestHandler state
previouslyExisted fn handler_ = (handler_ { previouslyExisted = Just fn })

-- | Add a forbidden callback to the provided RestHandler
forbidden :: forall state. (Req -> state -> Effect (RestResult Boolean state)) -> RestHandler state -> RestHandler state
forbidden fn handler_ = (handler_ { forbidden = Just fn })

-- | Create an init response for return from an InitHandler
initResult :: forall state. Req -> state -> Effect (InitResult state)
initResult rq st = pure $ InitOk rq st

-- | Create an rest response for return from a rest callback
result :: forall reply state. reply -> Req -> state -> Effect (RestResult reply state)
result re rq st = pure $ RestOk re rq st

-- | Finish defining this rest handler_, yeehaaw
yeeha :: forall state. RestHandler state -> StetsonHandler state
yeeha = Rest
