## Module Stetson

This is the entry point into the Stetson wrapper
You'll want to call Stetson.configure and then follow the types..

#### `configure`

``` purescript
configure :: StetsonConfig NoArguments
```

Creates a blank stetson config with default settings and no routes

#### `cowboyRoutes`

``` purescript
cowboyRoutes :: forall a. List Path -> StetsonConfig a -> StetsonConfig a
```

Introduce a list of native Erlang cowboy handlers to this config

#### `routes`

``` purescript
routes :: forall a b rep r. Generic a rep => GDispatch rep r => RouteDuplex' a -> Record r -> StetsonConfig b -> StetsonConfig a
```

#### `routes2`

``` purescript
routes2 :: forall a rep r. Generic a rep => GDispatch rep r => RouteDuplex' a -> Record r -> RouteConfig a
```

#### `port`

``` purescript
port :: forall a. Int -> StetsonConfig a -> StetsonConfig a
```

Set the port that this http listener will listen to

#### `bindTo`

``` purescript
bindTo :: forall a. Int -> Int -> Int -> Int -> StetsonConfig a -> StetsonConfig a
```

Set the IP that this http listener will bind to (default: 0.0.0.0)

#### `streamHandlers`

``` purescript
streamHandlers :: forall a. List NativeModuleName -> StetsonConfig a -> StetsonConfig a
```

Supply a list of modules to act as native stream handlers in cowboy

#### `middlewares`

``` purescript
middlewares :: forall a. List NativeModuleName -> StetsonConfig a -> StetsonConfig a
```

Supply a list of modules to act as native middlewares in cowboy

#### `startClear`

``` purescript
startClear :: forall a. String -> StetsonConfig a -> Effect (Either Foreign Unit)
```

Start the listener with the specified name

#### `stop`

``` purescript
stop :: String -> Effect Unit
```


### Re-exported from Stetson.Types:

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

#### `StetsonHandler`

``` purescript
data StetsonHandler msg state
  = StetsonHandler (StetsonHandlerCallbacks msg state)
```

A builder containing the complete set of callbacks for any sort of request

#### `StetsonConfig`

``` purescript
type StetsonConfig a = { bindAddress :: Tuple4 Int Int Int Int, bindPort :: Int, cowboyRoutes :: List Path, middlewares :: Maybe (List NativeModuleName), routes :: RouteConfig a, streamHandlers :: Maybe (List NativeModuleName) }
```

#### `StaticAssetLocation`

``` purescript
data StaticAssetLocation
  = PrivDir String String
  | PrivFile String String
```

#### `SimpleStetsonHandler`

``` purescript
type SimpleStetsonHandler state = StetsonHandler Unit state
```

A type alias for StetsonHandler, but with no ability to receive messages

#### `RouteHandler`

``` purescript
data RouteHandler
  = StetsonRoute (Exists StetsonRouteInner)
  | StaticRoute (Array String) StaticAssetLocation
  | CowboyRouteFallthrough
```

#### `RouteConfig`

``` purescript
type RouteConfig a = { dispatch :: a -> RouteHandler, routing :: RouteDuplex' a }
```

#### `RestResult`

``` purescript
data RestResult reply state
  = RestOk reply Req state
  | RestStop Req state
  | RestSwitch CowboyHandler Req state
```

The return type of most of the callbacks invoked as part of the REST workflow

#### `ProvideHandler`

``` purescript
type ProvideHandler state = Req -> state -> Effect (RestResult String state)
```

A callback invoked to 'provide' a specific content type

#### `LoopCallResult`

``` purescript
data LoopCallResult state
  = LoopOk Req state
  | LoopHibernate Req state
  | LoopStop Req state
```

Return type of most Loop callbacks

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

#### `HandlerArgs`

``` purescript
data HandlerArgs :: Type
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

#### `Authorized`

``` purescript
data Authorized
  = Authorized
  | NotAuthorized String
```

Return type of the isAuthorized callback

#### `AcceptHandler`

``` purescript
type AcceptHandler state = Req -> state -> Effect (RestResult Boolean state)
```

A callback invoked to 'accept' a specific content type

#### `runStetsonRoute`

``` purescript
runStetsonRoute :: forall z. (forall b c. StetsonHandler b c -> z) -> Exists StetsonRouteInner -> z
```

#### `mkStetsonRoute`

``` purescript
mkStetsonRoute :: forall a s. StetsonHandler a s -> Exists StetsonRouteInner
```

#### `emptyHandler`

``` purescript
emptyHandler :: forall msg state. InitHandler state -> StetsonHandler msg state
```

