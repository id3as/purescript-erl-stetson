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
                    , preHook'
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
import Stetson.Types (AcceptHandler, Authorized, HttpMethod, InitHandler, InitResult(..), ProvideHandler, StetsonHandlerCallbacks, RestResult(..), StetsonHandlerCallbacks(..), InnerStetsonHandler(..), emptyHandler)

-- | Create a cowboy REST handler with the provided Init handler and no callbacks defined
handler :: forall state. InitHandler state -> StetsonHandlerCallbacks Unit state
handler = emptyHandler

-- | Add an allowedMethods callback to the provided StetsonHandlerCallbacks
allowedMethods :: forall msg state. (Req -> state -> Effect (RestResult (List HttpMethod) state)) -> StetsonHandlerCallbacks msg state -> StetsonHandlerCallbacks msg state
allowedMethods fn handler_ = (handler_ { allowedMethods = Just fn })

-- | Add an malformedRequest callback to the provided StetsonHandlerCallbacks
malformedRequest :: forall msg state. (Req -> state -> Effect (RestResult Boolean state)) -> StetsonHandlerCallbacks msg state -> StetsonHandlerCallbacks msg state
malformedRequest fn handler_ = (handler_ { malformedRequest = Just fn })

-- | Add a resourceExists callback to the provided StetsonHandlerCallbacks
resourceExists :: forall msg state. (Req -> state -> Effect (RestResult Boolean state)) -> StetsonHandlerCallbacks msg state -> StetsonHandlerCallbacks msg state
resourceExists fn handler_ = (handler_ { resourceExists = Just fn })

-- | Add an isAuthorized callback to the provided StetsonHandlerCallbacks
isAuthorized :: forall msg state. (Req -> state -> Effect (RestResult Authorized state)) -> StetsonHandlerCallbacks msg state -> StetsonHandlerCallbacks msg state
isAuthorized fn handler_ = (handler_ { isAuthorized = Just fn })

-- | Add an isConflict callback to the provided StetsonHandlerCallbacks
isConflict :: forall msg state. (Req -> state -> Effect (RestResult Boolean state)) -> StetsonHandlerCallbacks msg state -> StetsonHandlerCallbacks msg state
isConflict fn handler_ = (handler_ { isConflict = Just fn })

-- | Add a contentTypesAccepted callback to the provided StetsonHandlerCallbacks
contentTypesAccepted :: forall msg state. (Req -> state -> Effect (RestResult (List (Tuple2 String (AcceptHandler state))) state)) -> StetsonHandlerCallbacks msg state -> StetsonHandlerCallbacks msg state
contentTypesAccepted fn handler_ = (handler_ { contentTypesAccepted = Just fn  })

-- | Add a contentTypesProvided callback to the provided StetsonHandlerCallbacks
contentTypesProvided :: forall msg state. (Req -> state -> Effect (RestResult (List (Tuple2 String (ProvideHandler state))) state)) -> StetsonHandlerCallbacks msg state -> StetsonHandlerCallbacks msg state
contentTypesProvided fn handler_ = (handler_ { contentTypesProvided = Just fn  })

-- | Add a deleteResource callback to the provided StetsonHandlerCallbacks
deleteResource :: forall msg state. (Req -> state -> Effect (RestResult Boolean state)) -> StetsonHandlerCallbacks msg state -> StetsonHandlerCallbacks msg state
deleteResource fn handler_ = (handler_ { deleteResource = Just fn })

-- | Add a movedTemporarily callback to the provided StetsonHandlerCallbacks
movedTemporarily :: forall msg state. (Req -> state -> Effect (RestResult MovedResult state)) -> StetsonHandlerCallbacks msg state -> StetsonHandlerCallbacks msg state
movedTemporarily fn handler_ = (handler_ { movedTemporarily = Just fn })

-- | Add a movedPermanently callback to the provided StetsonHandlerCallbacks
movedPermanently :: forall msg state. (Req -> state -> Effect (RestResult MovedResult state)) -> StetsonHandlerCallbacks msg state -> StetsonHandlerCallbacks msg state
movedPermanently fn handler_ = (handler_ { movedPermanently = Just fn })

-- | Add a serviceAvailable callback to the provided StetsonHandlerCallbacks
serviceAvailable :: forall msg state. (Req -> state -> Effect (RestResult Boolean state)) -> StetsonHandlerCallbacks msg state -> StetsonHandlerCallbacks msg state
serviceAvailable fn handler_ = (handler_ { serviceAvailable = Just fn })

-- | Add a previouslyExisted callback to the provided StetsonHandlerCallbacks
previouslyExisted :: forall msg state. (Req -> state -> Effect (RestResult Boolean state)) -> StetsonHandlerCallbacks msg state -> StetsonHandlerCallbacks msg state
previouslyExisted fn handler_ = (handler_ { previouslyExisted = Just fn })

-- | Add a allowMissingPost callback to the provided StetsonHandlerCallbacks
allowMissingPost :: forall msg state. (Req -> state -> Effect (RestResult Boolean state)) -> StetsonHandlerCallbacks msg state -> StetsonHandlerCallbacks msg state
allowMissingPost fn handler_ = (handler_ { allowMissingPost = Just fn })

-- | Add a forbidden callback to the provided StetsonHandlerCallbacks
forbidden :: forall msg state. (Req -> state -> Effect (RestResult Boolean state)) -> StetsonHandlerCallbacks msg state -> StetsonHandlerCallbacks msg state
forbidden fn handler_ = (handler_ { forbidden = Just fn })

-- | Create an init response for return from an InitHandler
initResult :: forall msg state. Req -> state -> Effect (InitResult state)
initResult rq st = pure $ Rest rq st

-- | Create a rest response for return from a rest callback
result :: forall reply state. reply -> Req -> state -> Effect (RestResult reply state)
result re rq st = pure $ RestOk re rq st

-- | Create a rest stop response for return from a rest callback
stop :: forall reply state. Req -> state -> Effect (RestResult reply state)
stop rq st = pure $ RestStop rq st

-- | Finish defining this rest handler, yeehaaw
yeeha :: forall msg state. StetsonHandlerCallbacks msg state -> InnerStetsonHandler msg state
yeeha = Complete -- may not need this any more

--------------------------------------------------------------------------------
-- Debug helpers
--------------------------------------------------------------------------------
preHook :: forall msg state.
           (forall state2. String -> Req -> state2 -> Effect Unit)
             -> StetsonHandlerCallbacks msg state -> StetsonHandlerCallbacks msg state

preHook hook =
  preHook'
    \name orgHandler ->
      \req state -> do
        _ <- hook name req state
        orgHandler req state

-- | Add a hook in front of every call to a handler
preHook' :: forall msg state.
           (forall a state2. (String -> (Req -> state2 -> Effect a) -> (Req -> state2 -> Effect a)))
             -> StetsonHandlerCallbacks msg state -> StetsonHandlerCallbacks msg state
preHook' hook state =
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

  -- TODO: These
  , wsInit               : state.wsInit
  , handle               : state.handle
  , info                 : state.info
  , externalMapping      : state.externalMapping
  }
