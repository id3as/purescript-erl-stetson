-- | This module contains the functions necessary to define a rest handler for a route in Stetson/Cowboy
-- | This maps pretty much 1-1 onto https://ninenines.eu/docs/en/cowboy/2.5/guide/rest_handlers/#_callbacks
-- | Although only the handlers that we have needed so far are defined - feel free to send pull requests that add the ones you need
module Stetson.Rest
  ( handler
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
  , switchHandler
  , forbidden
  , terminate
  , initResult
  , result
  , stop
  , preHook
  , preHook'
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Foreign (Foreign)
import Effect (Effect)
import Erl.Cowboy.Handlers.Rest (MovedResult)
import Erl.Cowboy.Req (Req)
import Erl.Data.List (List)
import Erl.Data.Tuple (Tuple2)
import Stetson.Types (AcceptHandler, Authorized, CowboyHandler, HttpMethod, InitHandler, InitResult(..), ProvideHandler, RestResult(..), StetsonHandler(..), emptyHandler)

-- | Create a cowboy REST handler with the provided Init handler and no callbacks defined
handler :: forall state. InitHandler state -> StetsonHandler Unit state
handler = emptyHandler

-- | Add an allowedMethods callback to the provided StetsonHandler
allowedMethods :: forall msg state. (Req -> state -> Effect (RestResult (List HttpMethod) state)) -> StetsonHandler msg state -> StetsonHandler msg state
allowedMethods fn (StetsonHandler h) = (StetsonHandler $ h { allowedMethods = Just fn })

-- | Add an malformedRequest callback to the provided StetsonHandler
malformedRequest :: forall msg state. (Req -> state -> Effect (RestResult Boolean state)) -> StetsonHandler msg state -> StetsonHandler msg state
malformedRequest fn (StetsonHandler h) = (StetsonHandler $ h { malformedRequest = Just fn })

-- | Add a resourceExists callback to the provided StetsonHandler
resourceExists :: forall msg state. (Req -> state -> Effect (RestResult Boolean state)) -> StetsonHandler msg state -> StetsonHandler msg state
resourceExists fn (StetsonHandler h) = (StetsonHandler $ h { resourceExists = Just fn })

-- | Add an isAuthorized callback to the provided StetsonHandler
isAuthorized :: forall msg state. (Req -> state -> Effect (RestResult Authorized state)) -> StetsonHandler msg state -> StetsonHandler msg state
isAuthorized fn (StetsonHandler h) = (StetsonHandler $ h { isAuthorized = Just fn })

-- | Add an isConflict callback to the provided StetsonHandler
isConflict :: forall msg state. (Req -> state -> Effect (RestResult Boolean state)) -> StetsonHandler msg state -> StetsonHandler msg state
isConflict fn (StetsonHandler h) = (StetsonHandler $ h { isConflict = Just fn })

-- | Add a contentTypesAccepted callback to the provided StetsonHandler
contentTypesAccepted :: forall msg state. (Req -> state -> Effect (RestResult (List (Tuple2 String (AcceptHandler state))) state)) -> StetsonHandler msg state -> StetsonHandler msg state
contentTypesAccepted fn (StetsonHandler h) = (StetsonHandler $ h { contentTypesAccepted = Just fn })

-- | Add a contentTypesProvided callback to the provided StetsonHandler
contentTypesProvided :: forall msg state. (Req -> state -> Effect (RestResult (List (Tuple2 String (ProvideHandler state))) state)) -> StetsonHandler msg state -> StetsonHandler msg state
contentTypesProvided fn (StetsonHandler h) = (StetsonHandler $ h { contentTypesProvided = Just fn })

-- | Add a deleteResource callback to the provided StetsonHandler
deleteResource :: forall msg state. (Req -> state -> Effect (RestResult Boolean state)) -> StetsonHandler msg state -> StetsonHandler msg state
deleteResource fn (StetsonHandler h) = (StetsonHandler $ h { deleteResource = Just fn })

-- | Add a movedTemporarily callback to the provided StetsonHandler
movedTemporarily :: forall msg state. (Req -> state -> Effect (RestResult MovedResult state)) -> StetsonHandler msg state -> StetsonHandler msg state
movedTemporarily fn (StetsonHandler h) = (StetsonHandler $ h { movedTemporarily = Just fn })

-- | Add a movedPermanently callback to the provided StetsonHandler
movedPermanently :: forall msg state. (Req -> state -> Effect (RestResult MovedResult state)) -> StetsonHandler msg state -> StetsonHandler msg state
movedPermanently fn (StetsonHandler h) = (StetsonHandler $ h { movedPermanently = Just fn })

-- | Add a serviceAvailable callback to the provided StetsonHandler
serviceAvailable :: forall msg state. (Req -> state -> Effect (RestResult Boolean state)) -> StetsonHandler msg state -> StetsonHandler msg state
serviceAvailable fn (StetsonHandler h) = (StetsonHandler $ h { serviceAvailable = Just fn })

-- | Add a previouslyExisted callback to the provided StetsonHandler
previouslyExisted :: forall msg state. (Req -> state -> Effect (RestResult Boolean state)) -> StetsonHandler msg state -> StetsonHandler msg state
previouslyExisted fn (StetsonHandler h) = (StetsonHandler $ h { previouslyExisted = Just fn })

-- | Add a allowMissingPost callback to the provided StetsonHandler
allowMissingPost :: forall msg state. (Req -> state -> Effect (RestResult Boolean state)) -> StetsonHandler msg state -> StetsonHandler msg state
allowMissingPost fn (StetsonHandler h) = (StetsonHandler $ h { allowMissingPost = Just fn })

-- | Add a forbidden callback to the provided StetsonHandler
forbidden :: forall msg state. (Req -> state -> Effect (RestResult Boolean state)) -> StetsonHandler msg state -> StetsonHandler msg state
forbidden fn (StetsonHandler h) = (StetsonHandler $ h { forbidden = Just fn })

-- | Add a terminate callback to the provided StetsonHandler
terminate :: forall msg state. (Foreign -> Req -> state -> Effect Unit) -> StetsonHandler msg state -> StetsonHandler msg state
terminate fn (StetsonHandler h) = (StetsonHandler $ h { terminate = Just fn })

-- | Create an init response for return from an InitHandler
initResult :: forall state. Req -> state -> Effect (InitResult state)
initResult rq st = pure $ Rest rq st

-- | Create a rest response for return from a rest callback
result :: forall reply state. reply -> Req -> state -> Effect (RestResult reply state)
result re rq st = pure $ RestOk re rq st

-- | Switches to a different handler (probably cowboy_loop)
switchHandler :: forall reply state. CowboyHandler -> Req -> state -> Effect (RestResult reply state)
switchHandler handler rq st = pure $ RestSwitch handler rq st

-- | Create a rest stop response for return from a rest callback
stop :: forall reply state. Req -> state -> Effect (RestResult reply state)
stop rq st = pure $ RestStop rq st

--------------------------------------------------------------------------------
-- Debug helpers
--------------------------------------------------------------------------------
preHook ::
  forall msg state.
  (forall state2. String -> Req -> state2 -> Effect Unit) ->
  StetsonHandler msg state -> StetsonHandler msg state
preHook hook =
  preHook' \name orgHandler -> \req state -> do
    _ <- hook name req state
    orgHandler req state

-- | Add a hook in front of every call to a handler
preHook' ::
  forall msg state.
  (forall a state2. (String -> (Req -> state2 -> Effect a) -> (Req -> state2 -> Effect a))) ->
  StetsonHandler msg state -> StetsonHandler msg state
preHook' hook (StetsonHandler state) =
  StetsonHandler
    { init: state.init
    , terminate: state.terminate
    , allowedMethods: hook "allowedMethods" <$> state.allowedMethods
    , malformedRequest: hook "malformedRequest" <$> state.malformedRequest
    , resourceExists: hook "resourceExists" <$> state.resourceExists
    , contentTypesAccepted: hook "contentTypesAccepted" <$> state.contentTypesAccepted
    , contentTypesProvided: hook "contentTypesProvided" <$> state.contentTypesProvided
    , deleteResource: hook "deleteResource" <$> state.deleteResource
    , isAuthorized: hook "isAuthorized" <$> state.isAuthorized
    , isConflict: hook "isConflict" <$> state.isConflict
    , movedTemporarily: hook "movedTemporarily" <$> state.movedTemporarily
    , movedPermanently: hook "movedPermanently" <$> state.movedPermanently
    , serviceAvailable: hook "serviceAvailable" <$> state.serviceAvailable
    , previouslyExisted: hook "previouslyExisted" <$> state.previouslyExisted
    , allowMissingPost: hook "allowMissingPost" <$> state.allowMissingPost
    , forbidden: hook "forbidden" <$> state.forbidden
    -- TODO: These
    , wsInit: state.wsInit
    , wsHandle: state.wsHandle
    , wsInfo: state.wsInfo
    , loopInfo: state.loopInfo
    , loopInit: state.loopInit
    }
