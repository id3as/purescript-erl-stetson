## Module Stetson

This is the entry point into the Stetson wrapper
You'll want to call Stetson.configure and then follow the types..

#### `RestResult`

``` purescript
data RestResult reply state
  = RestOk reply Req state
```

The return type of most of the callbacks invoked as part of the REST workflow

#### `InitResult`

``` purescript
data InitResult state
  = InitOk Req state
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

#### `RestHandler`

``` purescript
type RestHandler state = { init :: Req -> Effect (InitResult state), allowedMethods :: Maybe (Req -> state -> Effect (RestResult (List HttpMethod) state)), resourceExists :: Maybe (Req -> state -> Effect (RestResult Boolean state)), contentTypesAccepted :: Maybe (Req -> state -> Effect (RestResult (List (Tuple2 String (AcceptHandler state))) state)), contentTypesProvided :: Maybe (Req -> state -> Effect (RestResult (List (Tuple2 String (ProvideHandler state))) state)), deleteResource :: Maybe (Req -> state -> Effect (RestResult Boolean state)), isAuthorized :: Maybe (Req -> state -> Effect (RestResult Authorized state)), movedTemporarily :: Maybe (Req -> state -> Effect (RestResult MovedResult state)), movedPermanently :: Maybe (Req -> state -> Effect (RestResult MovedResult state)), serviceAvailable :: Maybe (Req -> state -> Effect (RestResult Boolean state)), previouslyExisted :: Maybe (Req -> state -> Effect (RestResult Boolean state)), forbidden :: Maybe (Req -> state -> Effect (RestResult Boolean state)) }
```

A builder containing the complete set of callbacks during the rest workflow for a specific handler

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
data StetsonHandler state
  = Rest (RestHandler state)
```

#### `StaticAssetLocation`

``` purescript
data StaticAssetLocation
  = PrivDir String String
  | PrivFile String String
```

#### `StetsonRoute`

``` purescript
type StetsonRoute = { route :: String, moduleName :: NativeModuleName, args :: HandlerArgs }
```

#### `HandlerArgs`

``` purescript
data HandlerArgs :: Type
```

#### `ConfiguredRoute`

``` purescript
data ConfiguredRoute
  = Stetson StetsonRoute
  | Cowboy Path
```

#### `StetsonConfig`

``` purescript
type StetsonConfig = { bindPort :: Int, bindAddress :: Tuple4 Int Int Int Int, streamHandlers :: Maybe (List NativeModuleName), middlewares :: Maybe (List NativeModuleName), routes :: List ConfiguredRoute }
```

#### `configure`

``` purescript
configure :: StetsonConfig
```

Creates a blank stetson config with default settings and no routes

#### `route`

``` purescript
route :: forall state. String -> StetsonHandler state -> StetsonConfig -> StetsonConfig
```

Add a route to a StetsonConfig
value: The path this route will handle (this takes the same format as cowboy routes)
handler: The handler that will take care of this request
config: The config to add this route to
```purescript
let newConfig = Stetson.route "/items/:id" myHandler config
```

#### `static`

``` purescript
static :: String -> StaticAssetLocation -> StetsonConfig -> StetsonConfig
```

Add a static route handler to a StetsonConfig
This can either be a file or a directory to serve a file or files from

#### `cowboyRoutes`

``` purescript
cowboyRoutes :: List Path -> StetsonConfig -> StetsonConfig
```

Introduce a list of native Erlang cowboy handlers to this config

#### `port`

``` purescript
port :: Int -> StetsonConfig -> StetsonConfig
```

Set the port that this http listener will listen to

#### `bindTo`

``` purescript
bindTo :: Int -> Int -> Int -> Int -> StetsonConfig -> StetsonConfig
```

Set the IP that this http listener will bind to (default: 0.0.0.0)

#### `streamHandlers`

``` purescript
streamHandlers :: List NativeModuleName -> StetsonConfig -> StetsonConfig
```

Supply a list of modules to act as native stream handlers in cowboy

#### `middlewares`

``` purescript
middlewares :: List NativeModuleName -> StetsonConfig -> StetsonConfig
```

Supply a list of modules to act as native middlewares in cowboy

#### `startClear`

``` purescript
startClear :: String -> StetsonConfig -> Effect Unit
```

Start the listener with the specified name


