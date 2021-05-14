## Module Stetson.HandlerProxy

#### `self`

``` purescript
self :: forall msg. Effect (Process msg)
```

#### `ElidedInitResult`

``` purescript
data ElidedInitResult :: Type
```

#### `restInitResult`

``` purescript
restInitResult :: forall msg state. State msg state -> Req -> ElidedInitResult
```

#### `wsInitResult`

``` purescript
wsInitResult :: forall msg state. State msg state -> Req -> ElidedInitResult
```

#### `loopInitResult`

``` purescript
loopInitResult :: forall msg state. State msg state -> Req -> ElidedInitResult
```

#### `State`

``` purescript
type State msg state = { acceptHandlers :: List (Req -> state -> Effect (RestResult Boolean state)), handler :: StetsonHandlerCallbacks msg state, innerState :: state, provideHandlers :: List (Req -> state -> Effect (RestResult String state)) }
```

#### `InitHandler`

``` purescript
type InitHandler c s = EffectFn2 Req c (InitResult s)
```

#### `init`

``` purescript
init :: forall msg state. EffectFn2 Req (StetsonHandlerCallbacks msg state) ElidedInitResult
```

#### `terminate`

``` purescript
terminate :: forall msg state. EffectFn3 Foreign Req (State msg state) Atom
```

#### `resource_exists`

``` purescript
resource_exists :: forall msg state. ResourceExistsHandler (State msg state)
```

#### `allowed_methods`

``` purescript
allowed_methods :: forall msg state. AllowedMethodsHandler (State msg state)
```

#### `malformed_request`

``` purescript
malformed_request :: forall msg state. MalformedRequestHandler (State msg state)
```

#### `previously_existed`

``` purescript
previously_existed :: forall msg state. PreviouslyExistedHandler (State msg state)
```

#### `allow_missing_post`

``` purescript
allow_missing_post :: forall msg state. PreviouslyExistedHandler (State msg state)
```

#### `moved_permanently`

``` purescript
moved_permanently :: forall msg state. MovedPermanentlyHandler (State msg state)
```

#### `moved_temporarily`

``` purescript
moved_temporarily :: forall msg state. MovedTemporarilyHandler (State msg state)
```

#### `service_available`

``` purescript
service_available :: forall msg state. ServiceAvailableHandler (State msg state)
```

#### `is_authorized`

``` purescript
is_authorized :: forall msg state. IsAuthorizedHandler (State msg state)
```

#### `is_conflict`

``` purescript
is_conflict :: forall msg state. IsConflictHandler (State msg state)
```

#### `forbidden`

``` purescript
forbidden :: forall msg state. ForbiddenHandler (State msg state)
```

#### `delete_resource`

``` purescript
delete_resource :: forall msg state. DeleteResourceHandler (State msg state)
```

#### `content_types_accepted`

``` purescript
content_types_accepted :: forall msg state. ContentTypesAcceptedHandler (State msg state)
```

#### `content_types_provided`

``` purescript
content_types_provided :: forall msg state. ContentTypesProvidedHandler (State msg state)
```

#### `callMap`

``` purescript
callMap :: forall msg state reply mappedReply. (reply -> mappedReply) -> Maybe (Req -> state -> Effect (RestResult reply state)) -> Req -> (State msg state) -> Effect (RestResult mappedReply (State msg state))
```

#### `call`

``` purescript
call :: forall msg state reply. Maybe (Req -> state -> Effect (RestResult reply state)) -> Req -> State msg state -> Effect (RestResult reply (State msg state))
```

#### `mapReply`

``` purescript
mapReply :: forall state reply mappedReply. (reply -> mappedReply) -> Effect (RestResult reply state) -> Effect (RestResult mappedReply state)
```

#### `restResult`

``` purescript
restResult :: forall reply msg state. State msg state -> Maybe (Effect (RestResult reply state)) -> Effect (RestResult reply (State msg state))
```

#### `noCall`

``` purescript
noCall :: forall t3 t4. Applicative t3 => t3 t4
```

#### `accept`

``` purescript
accept :: forall msg state. Int -> EffectFn2 Req (State msg state) (RestResult Boolean (State msg state))
```

#### `provide`

``` purescript
provide :: forall msg state. Int -> EffectFn2 Req (State msg state) (RestResult String (State msg state))
```

#### `accept_0`

``` purescript
accept_0 :: forall msg state. EffectFn2 Req (State msg state) (RestResult Boolean (State msg state))
```

#### `accept_1`

``` purescript
accept_1 :: forall msg state. EffectFn2 Req (State msg state) (RestResult Boolean (State msg state))
```

#### `accept_2`

``` purescript
accept_2 :: forall msg state. EffectFn2 Req (State msg state) (RestResult Boolean (State msg state))
```

#### `accept_3`

``` purescript
accept_3 :: forall msg state. EffectFn2 Req (State msg state) (RestResult Boolean (State msg state))
```

#### `accept_4`

``` purescript
accept_4 :: forall msg state. EffectFn2 Req (State msg state) (RestResult Boolean (State msg state))
```

#### `accept_5`

``` purescript
accept_5 :: forall msg state. EffectFn2 Req (State msg state) (RestResult Boolean (State msg state))
```

#### `accept_6`

``` purescript
accept_6 :: forall msg state. EffectFn2 Req (State msg state) (RestResult Boolean (State msg state))
```

#### `provide_0`

``` purescript
provide_0 :: forall msg state. EffectFn2 Req (State msg state) (RestResult String (State msg state))
```

#### `provide_1`

``` purescript
provide_1 :: forall msg state. EffectFn2 Req (State msg state) (RestResult String (State msg state))
```

#### `provide_2`

``` purescript
provide_2 :: forall msg state. EffectFn2 Req (State msg state) (RestResult String (State msg state))
```

#### `provide_3`

``` purescript
provide_3 :: forall msg state. EffectFn2 Req (State msg state) (RestResult String (State msg state))
```

#### `provide_4`

``` purescript
provide_4 :: forall msg state. EffectFn2 Req (State msg state) (RestResult String (State msg state))
```

#### `provide_5`

``` purescript
provide_5 :: forall msg state. EffectFn2 Req (State msg state) (RestResult String (State msg state))
```

#### `provide_6`

``` purescript
provide_6 :: forall msg state. EffectFn2 Req (State msg state) (RestResult String (State msg state))
```

#### `wsState`

``` purescript
wsState :: forall msg. Effect (WebSocketInternalState msg)
```

#### `websocket_init`

``` purescript
websocket_init :: forall msg state. WSInitHandler (State msg state)
```

#### `router`

``` purescript
router :: forall msg. Pid -> msg -> Effect Unit
```

#### `websocket_handle`

``` purescript
websocket_handle :: forall msg state. FrameHandler (State msg state)
```

#### `websocket_info`

``` purescript
websocket_info :: forall msg state. InfoHandler msg (State msg state)
```

#### `transformWsResult`

``` purescript
transformWsResult :: forall msg state. State msg state -> WebSocketCallResult state -> Effect (CallResult (State msg state))
```

#### `loopState`

``` purescript
loopState :: forall msg. Effect (LoopInternalState msg)
```

#### `info`

``` purescript
info :: forall msg state. InfoHandler msg (State msg state)
```

#### `transformLoopResult`

``` purescript
transformLoopResult :: forall msg state. State msg state -> LoopCallResult state -> Effect (InfoResult (State msg state))
```

#### `applyLoopInit`

``` purescript
applyLoopInit :: forall msg state. StetsonHandlerCallbacks msg state -> Req -> state -> Effect state
```

#### `switchHandler`

``` purescript
switchHandler :: forall reply msg state. CowboyHandler -> Req -> State msg state -> Effect (RestResult reply (State msg state))
```


