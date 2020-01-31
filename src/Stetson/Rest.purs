-- | This module contains the functions necessary to define a rest handler for a route in Stetson/Cowboy
-- | This maps pretty much 1-1 onto https://ninenines.eu/docs/en/cowboy/2.5/guide/rest_handlers/#_callbacks
-- | Although only the handlers that we have needed so far are defined - feel free to send pull requests that add the ones you need
module Stetson.Rest ( handler
                    , allowMissingPost
                    , allowedMethods
                    , malformedRequest
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
                    , stop
                    , preHook
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
import Stetson (AcceptHandler, Authorized, HttpMethod, InitHandler, InitResult(..), InnerStetsonHandler(..), ProvideHandler, RestHandler, RestResult(..), StetsonHandler)

-- | Create a cowboy REST handler with the provided Init handler and no callbacks defined
handler :: forall state. InitHandler state -> RestHandler state
handler init = {
  init
  , allowedMethods       : Nothing
  , malformedRequest     : Nothing
  , resourceExists       : Nothing
  , contentTypesAccepted : Nothing
  , contentTypesProvided : Nothing
  , deleteResource       : Nothing
  , isAuthorized         : Nothing
  , isConflict           : Nothing
  , movedTemporarily     : Nothing
  , movedPermanently     : Nothing
  , serviceAvailable     : Nothing
  , previouslyExisted    : Nothing
  , allowMissingPost     : Nothing
  , forbidden            : Nothing
  }

-- | Add an allowedMethods callback to the provided RestHandler
allowedMethods :: forall state. (Req -> state -> Effect (RestResult (List HttpMethod) state)) -> RestHandler state -> RestHandler state
allowedMethods fn handler_ = (handler_ { allowedMethods = Just fn })

-- | Add an malformedRequest callback to the provided RestHandler
malformedRequest :: forall state. (Req -> state -> Effect (RestResult Boolean state)) -> RestHandler state -> RestHandler state
malformedRequest fn handler_ = (handler_ { malformedRequest = Just fn })

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

-- | Add a allowMissingPost callback to the provided RestHandler
allowMissingPost :: forall state. (Req -> state -> Effect (RestResult Boolean state)) -> RestHandler state -> RestHandler state
allowMissingPost fn handler_ = (handler_ { allowMissingPost = Just fn })

-- | Add a forbidden callback to the provided RestHandler
forbidden :: forall state. (Req -> state -> Effect (RestResult Boolean state)) -> RestHandler state -> RestHandler state
forbidden fn handler_ = (handler_ { forbidden = Just fn })

-- | Create an init response for return from an InitHandler
initResult :: forall state. Req -> state -> Effect (InitResult state)
initResult rq st = pure $ InitOk rq st

-- | Create a rest response for return from a rest callback
result :: forall reply state. reply -> Req -> state -> Effect (RestResult reply state)
result re rq st = pure $ RestOk re rq st

-- | Create a rest stop response for return from a rest callback
stop :: forall reply state. Req -> state -> Effect (RestResult reply state)
stop rq st = pure $ RestStop rq st

-- | Finish defining this rest handler_, yeehaaw
yeeha :: forall state. RestHandler state -> StetsonHandler state
yeeha = Rest



--------------------------------------------------------------------------------
-- Debug helpers
--------------------------------------------------------------------------------
-- | Add a hook in front of every call to a handler
preHook :: forall state.
           (forall a. (String -> (Req -> state -> Effect a) -> (Req -> state -> Effect a)))
             -> RestHandler state -> RestHandler state
preHook hook state =
  { init: state.init
  , allowedMethods       : hook "allowedMethods"       <$> state.allowedMethods
  , malformedRequest     : hook "malformedRequest"     <$> state.malformedRequest
  , resourceExists       : hook "resourceExists"       <$> state.resourceExists
  , contentTypesAccepted : hook "contentTypesAccepted" <$> state.contentTypesAccepted
  , contentTypesProvided : hook "contentTypesProvided" <$> state.contentTypesProvided
  , deleteResource       : hook "deleteResource"       <$> state.deleteResource
  , isAuthorized         : hook "isAuthorized"         <$> state.isAuthorized
  , isConflict           : hook "isConflict"           <$> state.isConflict
  , movedTemporarily     : hook "movedTemporarily"     <$> state.movedTemporarily
  , movedPermanently     : hook "movedPermanently"     <$> state.movedPermanently
  , serviceAvailable     : hook "serviceAvailable"     <$> state.serviceAvailable
  , previouslyExisted    : hook "previouslyExisted"    <$> state.previouslyExisted
  , allowMissingPost     : hook "allowMissingPost"     <$> state.allowMissingPost
  , forbidden            : hook "forbidden"            <$> state.forbidden
  }
