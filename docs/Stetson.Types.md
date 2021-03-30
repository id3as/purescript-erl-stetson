## Module Stetson.Types

#### `RestResult`

``` purescript
data RestResult reply state
  = RestOk reply Req state
  | RestStop Req state
  | RestSwitch CowboyHandler Req state
```

The return type of most of the callbacks invoked as part of the REST workflow

#### `InitResult`

``` purescript
data InitResult state
  = Rest Req state
  | WebSocket Req state
  | Loop Req state
```

The return type of the 'init' callback in the REST workflow

#### `InitHandler`

``` purescript
type InitHandler state = Req -> Effect (InitResult state)
```

The callback invoked to kick off the REST workflow

#### `AcceptHandler`

``` purescript
type AcceptHandler state = Req -> state -> Effect (RestResult Boolean state)
```

A callback invoked to 'accept' a specific content type

#### `ProvideHandler`

``` purescript
type ProvideHandler state = Req -> state -> Effect (RestResult String state)
```

A callback invoked to 'provide' a specific content type

#### `WebSocketInitHandler`

``` purescript
type WebSocketInitHandler msg state = state -> WebSocketResult msg (WebSocketCallResult state)
```

Callback used to kick off the WebSocket handler
This is a good time to get hold of 'self' and set up subscriptions

#### `WebSocketInfoHandler`

``` purescript
type WebSocketInfoHandler msg state = msg -> state -> WebSocketResult msg (WebSocketCallResult state)
```

Callback used to handle messages sent from Erlang (hopefully via the router) so they'll be of the right type

#### `WebSocketHandleHandler`

``` purescript
type WebSocketHandleHandler msg state = Frame -> state -> WebSocketResult msg (WebSocketCallResult state)
```

Callback used to handle messages sent from the client in the form of 'Frames' which will need
unpacking/decoding/parsing etc

#### `WebSocketResult`

``` purescript
type WebSocketResult msg r = StateT (WebSocketInternalState msg) Effect r
```

All of the Loop handlers take place in a StateT so we can do things like get the current pid

#### `WebSocketCallResult`

``` purescript
data WebSocketCallResult state
  = NoReply state
  | Hibernate state
  | Reply (List Frame) state
  | ReplyAndHibernate (List Frame) state
  | Stop state
```

Return type of most WebSocket callbacks

#### `WebSocketInternalState`

``` purescript
type WebSocketInternalState msg = Process msg
```

We'll probably end up with more in here than just the current pid..

#### `HttpMethod`

``` purescript
data HttpMethod
  = GET
  | POST
  | HEAD
  | OPTIONS
  | PUT
  | DELETE
```

or is it a verb

##### Instances
``` purescript
Show HttpMethod
```

#### `Authorized`

``` purescript
data Authorized
  = Authorized
  | NotAuthorized String
```

Return type of the isAuthorized callback

#### `StetsonHandler`

``` purescript
data StetsonHandler msg state
  = StetsonHandler (StetsonHandlerCallbacks msg state)
```

A builder containing the complete set of callbacks for any sort of request

#### `SimpleStetsonHandler`

``` purescript
type SimpleStetsonHandler state = StetsonHandler Unit state
```

A type alias for StetsonHandler, but with no ability to receive messages

#### `StetsonHandlerCallbacks`

``` purescript
type StetsonHandlerCallbacks msg state = { allowMissingPost :: Maybe (Req -> state -> Effect (RestResult Boolean state)), allowedMethods :: Maybe (Req -> state -> Effect (RestResult (List HttpMethod) state)), contentTypesAccepted :: Maybe (Req -> state -> Effect (RestResult (List (Tuple2 String (AcceptHandler state))) state)), contentTypesProvided :: Maybe (Req -> state -> Effect (RestResult (List (Tuple2 String (ProvideHandler state))) state)), deleteResource :: Maybe (Req -> state -> Effect (RestResult Boolean state)), forbidden :: Maybe (Req -> state -> Effect (RestResult Boolean state)), init :: Req -> Effect (InitResult state), isAuthorized :: Maybe (Req -> state -> Effect (RestResult Authorized state)), isConflict :: Maybe (Req -> state -> Effect (RestResult Boolean state)), loopInfo :: Maybe (LoopInfoHandler msg state), loopInit :: Maybe (LoopInitHandler msg state), malformedRequest :: Maybe (Req -> state -> Effect (RestResult Boolean state)), movedPermanently :: Maybe (Req -> state -> Effect (RestResult MovedResult state)), movedTemporarily :: Maybe (Req -> state -> Effect (RestResult MovedResult state)), previouslyExisted :: Maybe (Req -> state -> Effect (RestResult Boolean state)), resourceExists :: Maybe (Req -> state -> Effect (RestResult Boolean state)), serviceAvailable :: Maybe (Req -> state -> Effect (RestResult Boolean state)), terminate :: Maybe (Foreign -> Req -> state -> Effect Unit), wsHandle :: Maybe (WebSocketHandleHandler msg state), wsInfo :: Maybe (WebSocketInfoHandler msg state), wsInit :: Maybe (WebSocketInitHandler msg state) }
```

The built record containing callbacks for any sort of request

#### `StaticAssetLocation`

``` purescript
data StaticAssetLocation
  = PrivDir String String
  | PrivFile String String
```

#### `CowboyRoutePlaceholder`

``` purescript
data CowboyRoutePlaceholder
  = CowboyRoutePlaceholder
```

#### `HandlerArgs`

``` purescript
data HandlerArgs :: Type
```

#### `StetsonConfig`

``` purescript
type StetsonConfig a = { bindAddress :: Tuple4 Int Int Int Int, bindPort :: Int, cowboyRoutes :: List Path, middlewares :: Maybe (List NativeModuleName), routes :: RouteConfig a, streamHandlers :: Maybe (List NativeModuleName) }
```

#### `RouteConfig`

``` purescript
type RouteConfig a = { dispatch :: a -> RouteHandler, routing :: RouteDuplex' a }
```

#### `RouteHandler`

``` purescript
data RouteHandler
  = StetsonRoute (Exists StetsonRouteInner)
  | StaticRoute (Array String) StaticAssetLocation
  | CowboyRouteFallthrough
```

#### `RouteHandler2`

``` purescript
type RouteHandler2 msg state = Config state (OptionalConfig Maybe msg state)
```

#### `Config`

``` purescript
type Config state r = { init :: Req -> Effect (InitResult state) | r }
```

#### `OptionalConfig`

``` purescript
type OptionalConfig f msg state = (allowMissingPost :: RequestHandler f Boolean state, allowedMethods :: RequestHandler f (List HttpMethod) state, contentTypesAccepted :: RequestHandler f (List (Tuple2 String (AcceptHandler state))) state, contentTypesProvided :: RequestHandler f (List (Tuple2 String (ProvideHandler state))) state, deleteResource :: RequestHandler f Boolean state, forbidden :: RequestHandler f Boolean state, isAuthorized :: RequestHandler f Authorized state, isConflict :: RequestHandler f Boolean state, loopInfo :: f (LoopInfoHandler msg state), loopInit :: f (LoopInitHandler msg state), malformedRequest :: RequestHandler f Boolean state, movedPermanently :: RequestHandler f MovedResult state, movedTemporarily :: RequestHandler f MovedResult state, previouslyExisted :: RequestHandler f Boolean state, resourceExists :: RequestHandler f Boolean state, serviceAvailable :: RequestHandler f Boolean state, terminate :: f (Foreign -> Req -> state -> Effect Unit), wsHandle :: f (WebSocketHandleHandler msg state), wsInfo :: f (WebSocketInfoHandler msg state), wsInit :: f (WebSocketInitHandler msg state))
```

#### `RequestHandler`

``` purescript
type RequestHandler f resultType state = f (Req -> state -> Effect (RestResult resultType state))
```

#### `StetsonRouteInner`

``` purescript
newtype StetsonRouteInner a
```

#### `CowboyHandler`

``` purescript
data CowboyHandler
  = RestHandler
  | LoopHandler
  | WebSocketHandler
```

The different handlers exposed by Cowboy and loosely mapping onto the
Rest/Loop/WebSocket namespaces

#### `LoopInitHandler`

``` purescript
type LoopInitHandler msg state = Req -> state -> LoopResult msg state
```

Callback used to kick off the Loop handler, it is here where subscriptions should be
created, and in their callbacks the messages should be passed into the router for dealing with in the info callback

#### `LoopInfoHandler`

``` purescript
type LoopInfoHandler msg state = msg -> Req -> state -> LoopResult msg (LoopCallResult state)
```

Callback used to handle messages sent from Erlang (hopefully via the router) so they'll be of the right type

#### `LoopInternalState`

``` purescript
type LoopInternalState msg = Process msg
```

We'll probably end up with more in here than just the current pid..

#### `LoopResult`

``` purescript
type LoopResult msg r = StateT (LoopInternalState msg) Effect r
```

All of the Loop handlers take place in a StateT so we can do things like get the current pid

#### `LoopCallResult`

``` purescript
data LoopCallResult state
  = LoopOk Req state
  | LoopHibernate Req state
  | LoopStop Req state
```

Return type of most Loop callbacks

#### `mkStetsonRoute`

``` purescript
mkStetsonRoute :: forall a s. StetsonHandler a s -> Exists StetsonRouteInner
```

#### `runStetsonRoute`

``` purescript
runStetsonRoute :: forall z. (forall b c. StetsonHandler b c -> z) -> Exists StetsonRouteInner -> z
```

#### `emptyHandler`

``` purescript
emptyHandler :: forall msg state. InitHandler state -> StetsonHandler msg state
```

#### `routeHandler`

``` purescript
routeHandler :: forall optional trash msg state. Union optional trash (OptionalConfig Unlift msg state) => Config state optional -> StetsonHandler msg state
```


