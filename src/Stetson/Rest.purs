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
                    , switchHandler
                    , forbidden
                    , initResult
                    , result
                    , stop
                    , preHook
                    , preHook'
                    )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Cowboy.Handlers.Rest (MovedResult, switchHandler)
import Erl.Cowboy.Req (Req)
import Erl.Data.List (List)
import Erl.Data.Tuple (Tuple2)
import Stetson.Types (AcceptHandler, Authorized, CowboyHandler, HttpMethod, InitHandler, InitResult(..), ProvideHandler, RestResult(..), StetsonHandlerBuilder, StetsonHandlerBuilder(..), emptyHandler)

-- | Create a cowboy REST handler with the provided Init handler and no callbacks defined
handler :: forall state. InitHandler state -> StetsonHandlerBuilder Unit state
handler = emptyHandler

-- | Add an allowedMethods callback to the provided StetsonHandlerBuilder
allowedMethods :: forall msg state. (Req -> state -> Effect (RestResult (List HttpMethod) state)) -> StetsonHandlerBuilder msg state -> StetsonHandlerBuilder msg state
allowedMethods fn (StetsonHandlerBuilder h) = (StetsonHandlerBuilder $ h { allowedMethods = Just fn })

-- | Add an malformedRequest callback to the provided StetsonHandlerBuilder
malformedRequest :: forall msg state. (Req -> state -> Effect (RestResult Boolean state)) -> StetsonHandlerBuilder msg state -> StetsonHandlerBuilder msg state
malformedRequest fn (StetsonHandlerBuilder h) = (StetsonHandlerBuilder $ h { malformedRequest = Just fn })

-- | Add a resourceExists callback to the provided StetsonHandlerBuilder
resourceExists :: forall msg state. (Req -> state -> Effect (RestResult Boolean state)) -> StetsonHandlerBuilder msg state -> StetsonHandlerBuilder msg state
resourceExists fn (StetsonHandlerBuilder h) = (StetsonHandlerBuilder $ h { resourceExists = Just fn })

-- | Add an isAuthorized callback to the provided StetsonHandlerBuilder
isAuthorized :: forall msg state. (Req -> state -> Effect (RestResult Authorized state)) -> StetsonHandlerBuilder msg state -> StetsonHandlerBuilder msg state
isAuthorized fn (StetsonHandlerBuilder h) = (StetsonHandlerBuilder $ h { isAuthorized = Just fn })

-- | Add an isConflict callback to the provided StetsonHandlerBuilder
isConflict :: forall msg state. (Req -> state -> Effect (RestResult Boolean state)) -> StetsonHandlerBuilder msg state -> StetsonHandlerBuilder msg state
isConflict fn (StetsonHandlerBuilder h) = (StetsonHandlerBuilder $ h { isConflict = Just fn })

-- | Add a contentTypesAccepted callback to the provided StetsonHandlerBuilder
contentTypesAccepted :: forall msg state. (Req -> state -> Effect (RestResult (List (Tuple2 String (AcceptHandler state))) state)) -> StetsonHandlerBuilder msg state -> StetsonHandlerBuilder msg state
contentTypesAccepted fn (StetsonHandlerBuilder h) = (StetsonHandlerBuilder $ h { contentTypesAccepted = Just fn  })

-- | Add a contentTypesProvided callback to the provided StetsonHandlerBuilder
contentTypesProvided :: forall msg state. (Req -> state -> Effect (RestResult (List (Tuple2 String (ProvideHandler state))) state)) -> StetsonHandlerBuilder msg state -> StetsonHandlerBuilder msg state
contentTypesProvided fn (StetsonHandlerBuilder h) = (StetsonHandlerBuilder $ h { contentTypesProvided = Just fn  })

-- | Add a deleteResource callback to the provided StetsonHandlerBuilder
deleteResource :: forall msg state. (Req -> state -> Effect (RestResult Boolean state)) -> StetsonHandlerBuilder msg state -> StetsonHandlerBuilder msg state
deleteResource fn (StetsonHandlerBuilder h) = (StetsonHandlerBuilder $ h { deleteResource = Just fn })

-- | Add a movedTemporarily callback to the provided StetsonHandlerBuilder
movedTemporarily :: forall msg state. (Req -> state -> Effect (RestResult MovedResult state)) -> StetsonHandlerBuilder msg state -> StetsonHandlerBuilder msg state
movedTemporarily fn (StetsonHandlerBuilder h) = (StetsonHandlerBuilder $ h { movedTemporarily = Just fn })

-- | Add a movedPermanently callback to the provided StetsonHandlerBuilder
movedPermanently :: forall msg state. (Req -> state -> Effect (RestResult MovedResult state)) -> StetsonHandlerBuilder msg state -> StetsonHandlerBuilder msg state
movedPermanently fn (StetsonHandlerBuilder h) = (StetsonHandlerBuilder $ h { movedPermanently = Just fn })

-- | Add a serviceAvailable callback to the provided StetsonHandlerBuilder
serviceAvailable :: forall msg state. (Req -> state -> Effect (RestResult Boolean state)) -> StetsonHandlerBuilder msg state -> StetsonHandlerBuilder msg state
serviceAvailable fn (StetsonHandlerBuilder h) = (StetsonHandlerBuilder $ h { serviceAvailable = Just fn })

-- | Add a previouslyExisted callback to the provided StetsonHandlerBuilder
previouslyExisted :: forall msg state. (Req -> state -> Effect (RestResult Boolean state)) -> StetsonHandlerBuilder msg state -> StetsonHandlerBuilder msg state
previouslyExisted fn (StetsonHandlerBuilder h) = (StetsonHandlerBuilder $ h { previouslyExisted = Just fn })

-- | Add a allowMissingPost callback to the provided StetsonHandlerBuilder
allowMissingPost :: forall msg state. (Req -> state -> Effect (RestResult Boolean state)) -> StetsonHandlerBuilder msg state -> StetsonHandlerBuilder msg state
allowMissingPost fn (StetsonHandlerBuilder h) = (StetsonHandlerBuilder $ h { allowMissingPost = Just fn })

-- | Add a forbidden callback to the provided StetsonHandlerBuilder
forbidden :: forall msg state. (Req -> state -> Effect (RestResult Boolean state)) -> StetsonHandlerBuilder msg state -> StetsonHandlerBuilder msg state
forbidden fn (StetsonHandlerBuilder h) = (StetsonHandlerBuilder $ h { forbidden = Just fn })

-- | Create an init response for return from an InitHandler
initResult :: forall msg state. Req -> state -> Effect (InitResult state)
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
preHook :: forall msg state.
           (forall state2. String -> Req -> state2 -> Effect Unit)
             -> StetsonHandlerBuilder msg state -> StetsonHandlerBuilder msg state

preHook hook =
  preHook'
    \name orgHandler ->
      \req state -> do
        _ <- hook name req state
        orgHandler req state

-- | Add a hook in front of every call to a handler
preHook' :: forall msg state.
           (forall a state2. (String -> (Req -> state2 -> Effect a) -> (Req -> state2 -> Effect a)))
             -> StetsonHandlerBuilder msg state -> StetsonHandlerBuilder msg state
preHook' hook (StetsonHandlerBuilder state) =
  StetsonHandlerBuilder { init: state.init
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
                        , wsHandle             : state.wsHandle
                        , wsInfo               : state.wsInfo
                        , loopInfo             : state.loopInfo
                        , loopInit             : state.loopInit
                        }
