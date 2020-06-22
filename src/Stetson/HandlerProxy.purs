module Stetson.HandlerProxy where

import Prelude

import Data.Compactable (applyMaybe)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1, mkEffectFn2)
import Erl.Process.Raw (Pid, send)
import Foreign (Foreign)
import Stetson (InitResult(..), WebSocketCallResult(..))
import Unsafe.Coerce (unsafeCoerce)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn2, mkEffectFn2)
import Erl.Atom (atom)
import Erl.Cowboy.Handlers.Rest (RestResult, restResult, stop, switchHandler) as Cowboy
import Erl.Cowboy.Req (Req)
import Erl.Data.List (List, mapWithIndex, nil, (!!))
import Erl.Data.Tuple (tuple2, uncurry2)
import Stetson.Types (Authorized(..), InitResult(..), StetsonHandlerCallbacks(..), RestResult(..), InitHandler, CowboyHandler(..))
import Erl.ModuleName (NativeModuleName(..))
import Unsafe.Coerce (unsafeCoerce)

import Erl.Cowboy.Handlers.WebSocket as CowboyWS
import Erl.Cowboy.Handlers.Rest as CowboyRest

foreign import self :: Effect Pid
foreign import data ElidedInitResult :: Type
foreign import restInitResult :: forall state. state -> Req ->  ElidedInitResult
foreign import wsInitResult :: forall state. state -> Req ->  ElidedInitResult

type State msg state =
  { handler :: StetsonHandlerCallbacks msg state
  , innerState :: state
  , acceptHandlers :: List (Req -> state -> Effect (RestResult Boolean state))
  , provideHandlers :: List (Req -> state -> Effect (RestResult String state))
  }

type InitHandler c s = EffectFn2 Req c (InitResult s)

init :: forall msg state. EffectFn2 Req (StetsonHandlerCallbacks msg state) ElidedInitResult
init = mkEffectFn2 \req handler -> do
  res <- handler.init req
  case  res of
       (Rest req2 innerState) -> pure $ restInitResult { handler, innerState } req
       (WebSocket req2 innerState) ->  pure $ wsInitResult { handler, innerState } req

--
-- Rest handler
--


resource_exists :: forall msg state. CowboyRest.ResourceExistsHandler (State msg state)
resource_exists = mkEffectFn2 \req state@{ handler } -> do
  call handler.resourceExists req state

allowed_methods :: forall msg state. CowboyRest.AllowedMethodsHandler (State msg state)
allowed_methods = mkEffectFn2 \req state@{ handler, innerState } -> do
  callMap (map show) handler.allowedMethods req state

malformed_request :: forall msg state. CowboyRest.MalformedRequestHandler (State msg state)
malformed_request = mkEffectFn2 \req state@{ handler, innerState } -> do
  call handler.malformedRequest req state

previously_existed :: forall msg state. CowboyRest.PreviouslyExistedHandler (State msg state)
previously_existed = mkEffectFn2 \req state@{ handler, innerState } -> do
  call handler.previouslyExisted req state

allow_missing_post :: forall msg state. CowboyRest.PreviouslyExistedHandler (State msg state)
allow_missing_post = mkEffectFn2 \req state@{ handler, innerState } -> do
  call handler.allowMissingPost req state

moved_permanently :: forall msg state. CowboyRest.MovedPermanentlyHandler (State msg state)
moved_permanently = mkEffectFn2 \req state@{ handler, innerState } -> do
  call handler.movedPermanently req state

moved_temporarily :: forall msg state. CowboyRest.MovedTemporarilyHandler (State msg state)
moved_temporarily = mkEffectFn2 \req state@{ handler, innerState } -> do
  call handler.movedTemporarily req state

service_available :: forall msg state. CowboyRest.ServiceAvailableHandler (State msg state)
service_available = mkEffectFn2 \req state@{ handler, innerState } -> do
  call handler.serviceAvailable req state

is_authorized :: forall msg state. CowboyRest.IsAuthorizedHandler (State msg state)
is_authorized = mkEffectFn2 \req state@{ handler } -> do
  callMap convertAuth handler.isAuthorized req state
  where
  convertAuth Authorized = CowboyRest.authorized
  convertAuth (NotAuthorized s) = CowboyRest.unauthorized s

is_conflict :: forall msg state. CowboyRest.IsConflictHandler (State msg state)
is_conflict = mkEffectFn2 \req state@{ handler } ->
  call handler.isConflict req state

forbidden :: forall msg state. CowboyRest.ForbiddenHandler (State msg state)
forbidden = mkEffectFn2 \req state@{ handler } ->
  call handler.forbidden req state

delete_resource :: forall msg state. CowboyRest.DeleteResourceHandler (State msg state)
delete_resource = mkEffectFn2 \req state@{ handler } -> do
  call handler.deleteResource req state

-- { "application", "json", call_foo }
-- { "application/json", call_foo }
-- { '*', call_foo }

content_types_accepted :: forall msg state. CowboyRest.ContentTypesAcceptedHandler (State msg state)
content_types_accepted = mkEffectFn2 \req state@{ handler, innerState } ->
  case handler.contentTypesAccepted of
       Nothing -> noCall
       Just factory -> do
          factoryResp <- factory req innerState
          case factoryResp of
            RestSwitch handler rq st -> pure $ Cowboy.switchHandler ((NativeModuleName <<< atom) $ case handler of
                                                                                   RestHandler -> "cowboy_rest"
                                                                                   LoopHandler -> "cowboy_loop"
                                                                                   WebSocketHandler -> "cowboy_websocket") 
                                                            (state { innerState = st }) rq
            RestOk callbacks req2 innerState2 ->
              let
                fns = map (\tuple -> uncurry2 (\ct fn -> fn) tuple) callbacks
                atoms = mapWithIndex (\tuple i -> uncurry2 (\ct _ -> tuple2 (CowboyRest.SimpleContentType ct) $ CowboyRest.AcceptCallback $ atom $ "accept_" <> show i) tuple) callbacks
              in
                pure $ Cowboy.restResult (CowboyRest.contentTypesAcceptedResult atoms) (state { innerState = innerState2, acceptHandlers = fns }) req2
            RestStop req2 innerState2 ->
              pure $ Cowboy.stop (state { innerState = innerState2}) req2


-- TODO: iodata/stream/etc
content_types_provided :: forall msg state. CowboyRest.ContentTypesProvidedHandler (State msg state)
content_types_provided = mkEffectFn2 \req state@{ handler, innerState } ->
  case handler.contentTypesProvided of
       Nothing -> noCall
       Just factory -> do
          factoryResp <- factory req innerState
          case factoryResp of
            RestSwitch handler rq st -> pure $ Cowboy.switchHandler ((NativeModuleName <<< atom) $ case handler of
                                                                                   RestHandler -> "cowboy_rest"
                                                                                   LoopHandler -> "cowboy_loop"
                                                                                   WebSocketHandler -> "cowboy_websocket") 
                                                            (state { innerState = st }) rq
            RestOk callbacks req2 innerState2 ->
              let
                fns = map (\tuple -> uncurry2 (\ct fn -> fn) tuple) callbacks
                atoms = mapWithIndex (\tuple i -> uncurry2 (\ct _ -> tuple2 (CowboyRest.SimpleContentType ct) $ CowboyRest.ProvideCallback $ atom $ "provide_" <> show i) tuple) callbacks
              in
                pure $ Cowboy.restResult (CowboyRest.contentTypesProvidedResult atoms) (state { innerState = innerState2, provideHandlers = fns }) req2
            RestStop req2 innerState2 ->
                pure $ Cowboy.stop (state { innerState = innerState2}) req2

callMap :: forall msg state reply mappedReply. (reply -> mappedReply) -> Maybe (Req -> state -> Effect (RestResult reply state)) -> Req -> (State msg state) -> Effect (Cowboy.RestResult mappedReply (State msg  state))
callMap mapFn fn req state = restResult state $ map (mapReply mapFn) $ fn <*> pure req <*> pure state.innerState

call :: forall msg state reply. Maybe (Req -> state -> Effect (RestResult reply state)) -> Req -> State msg state -> Effect (Cowboy.RestResult reply (State msg state))
call fn req state = restResult state $ fn <*> pure req <*> pure state.innerState

mapReply :: forall msg state reply mappedReply. (reply -> mappedReply) -> Effect (RestResult reply state) -> Effect (RestResult mappedReply state)
mapReply mapFn org = do
  orgResult <- org
  case orgResult of
    RestSwitch handler rq st -> pure $ RestSwitch handler rq st
    RestOk re rq st -> pure $ RestOk (mapFn re) rq st
    RestStop rq st -> pure $ RestStop rq st

restResult :: forall reply msg state. State msg state -> Maybe (Effect (RestResult reply state)) -> Effect (Cowboy.RestResult reply (State msg state))
restResult outerState (Just callback) = do
  result <- callback
  case result of
    RestSwitch handler rq st -> pure $ Cowboy.switchHandler ((NativeModuleName <<< atom) $ case handler of
                                                                                   RestHandler -> "cowboy_rest"
                                                                                   LoopHandler -> "cowboy_loop"
                                                                                   WebSocketHandler -> "cowboy_websocket") 
                                                            (outerState { innerState = st }) rq
    RestOk re rq st -> pure $ Cowboy.restResult re (outerState { innerState = st }) rq
    RestStop rq st ->
      do
        pure $ Cowboy.stop (outerState { innerState = st }) rq

-- This is an internal Cowboy detail, and we *must not* use the value from this once we've returned it
-- Cowboy may not support this in the future, but hopefully it will - essentially it means that
-- The function is entirely ignored and is therefore treated as optional
restResult outerState Nothing = noCall

noCall :: forall t3 t4. Applicative t3 => t3 t4
noCall = pure $ unsafeCoerce (atom "no_call")

accept :: forall msg state. Int -> EffectFn2 Req (State msg state) (Cowboy.RestResult Boolean (State msg state))
accept i = mkEffectFn2 \req state@{ acceptHandlers } ->
  call (acceptHandlers !! i) req state

provide :: forall msg state. Int -> EffectFn2 Req (State msg state) (Cowboy.RestResult String (State msg state))
provide i = mkEffectFn2 \req state@{ provideHandlers } ->
  call (provideHandlers !! i) req state


accept_0 :: forall msg state. EffectFn2 Req (State msg state) (Cowboy.RestResult Boolean (State msg state))
accept_0 = accept 0

accept_1 :: forall msg state. EffectFn2 Req (State msg state) (Cowboy.RestResult Boolean (State msg state))
accept_1 = accept 1

accept_2 :: forall msg state. EffectFn2 Req (State msg state) (Cowboy.RestResult Boolean (State msg state))
accept_2 = accept 2

accept_3 :: forall msg state. EffectFn2 Req (State msg state) (Cowboy.RestResult Boolean (State msg state))
accept_3 = accept 3

accept_4 :: forall msg state. EffectFn2 Req (State msg state) (Cowboy.RestResult Boolean (State msg state))
accept_4 = accept 4

accept_5 :: forall msg state. EffectFn2 Req (State msg state) (Cowboy.RestResult Boolean (State msg state))
accept_5 = accept 5

accept_6 :: forall msg state. EffectFn2 Req (State msg state) (Cowboy.RestResult Boolean (State msg state))
accept_6 = accept 6

provide_0 :: forall msg state. EffectFn2 Req (State msg state) (Cowboy.RestResult String (State msg state))
provide_0 = provide 0

provide_1 :: forall msg state. EffectFn2 Req (State msg state) (Cowboy.RestResult String (State msg state))
provide_1 = provide 1

provide_2 :: forall msg state. EffectFn2 Req (State msg state) (Cowboy.RestResult String (State msg state))
provide_2 = provide 2

provide_3 :: forall msg state. EffectFn2 Req (State msg state) (Cowboy.RestResult String (State msg state))
provide_3 = provide 3

provide_4 :: forall msg state. EffectFn2 Req (State msg state) (Cowboy.RestResult String (State msg state))
provide_4 = provide 4

provide_5 :: forall msg state. EffectFn2 Req (State msg state) (Cowboy.RestResult String (State msg state))
provide_5 = provide 5

provide_6 :: forall msg state. EffectFn2 Req (State msg state) (Cowboy.RestResult String (State msg state))
provide_6 = provide 6

--
-- Websocket handler
--


websocket_init :: forall msg state. CowboyWS.WSInitHandler (State msg state)
websocket_init = mkEffectFn1 \state -> do
  case state of
    { innerState, handler: { wsInit: Just wsInit }} -> do
      pid <- self
      transformResult state =<< wsInit (router pid) innerState
    _ ->
      pure $ CowboyWS.okResult state

router :: forall msg. Pid -> msg -> Effect Unit
router pid msg = do
  _ <- send pid msg
  pure unit

websocket_handle :: forall msg state. CowboyWS.FrameHandler (State msg state)
websocket_handle = mkEffectFn2 \frame state ->
  case state of
    { innerState, handler: { handle: Just handle }} ->
      transformResult state =<< handle (CowboyWS.decodeInFrame frame) innerState
    _ ->
      pure $ CowboyWS.okResult state

websocket_info :: forall msg state. CowboyWS.InfoHandler Foreign (State msg state)
websocket_info = mkEffectFn2 \foreignMsg state ->
  case state of
    { innerState, handler: { info: Just info
                           , externalMapping: maybeExternalMapping }} ->
      let
        mappedMsg = fromMaybe (unsafeCoerce foreignMsg) $ applyMaybe maybeExternalMapping (Just foreignMsg)
      in
        transformResult state =<< info mappedMsg innerState
    _ ->
      pure $ CowboyWS.okResult state

transformResult :: forall msg state. State msg state -> WebSocketCallResult state -> Effect (CowboyWS.CallResult (State msg state))
transformResult state result =
  case result of
       NoReply innerState -> pure $ CowboyWS.okResult $ state { innerState = innerState  }
       Hibernate innerState -> pure $ CowboyWS.hibernateResult $ state { innerState = innerState }
       Reply frames innerState ->  pure $ CowboyWS.replyResult  state { innerState = innerState }$ map CowboyWS.outFrame frames
       ReplyAndHibernate frames innerState -> pure $ CowboyWS.replyAndHibernateResult state { innerState = innerState } $ map CowboyWS.outFrame frames
       Stop innerState -> pure $ CowboyWS.stopResult state { innerState = innerState }
