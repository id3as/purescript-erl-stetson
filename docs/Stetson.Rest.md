## Module Stetson.Rest

This module contains the functions necessary to define a rest handler for a route in Stetson/Cowboy
This maps pretty much 1-1 onto https://ninenines.eu/docs/en/cowboy/2.5/guide/rest_handlers/#_callbacks
Although only the handlers that we have needed so far are defined - feel free to send pull requests that add the ones you need

#### `handler`

``` purescript
handler :: forall state. InitHandler state -> StetsonHandler Unit state
```

Create a cowboy REST handler with the provided Init handler and no callbacks defined

#### `allowMissingPost`

``` purescript
allowMissingPost :: forall msg state. (Req -> state -> Effect (RestResult Boolean state)) -> StetsonHandler msg state -> StetsonHandler msg state
```

Add a allowMissingPost callback to the provided StetsonHandler

#### `allowedMethods`

``` purescript
allowedMethods :: forall msg state. (Req -> state -> Effect (RestResult (List HttpMethod) state)) -> StetsonHandler msg state -> StetsonHandler msg state
```

Add an allowedMethods callback to the provided StetsonHandler

#### `malformedRequest`

``` purescript
malformedRequest :: forall msg state. (Req -> state -> Effect (RestResult Boolean state)) -> StetsonHandler msg state -> StetsonHandler msg state
```

Add an malformedRequest callback to the provided StetsonHandler

#### `resourceExists`

``` purescript
resourceExists :: forall msg state. (Req -> state -> Effect (RestResult Boolean state)) -> StetsonHandler msg state -> StetsonHandler msg state
```

Add a resourceExists callback to the provided StetsonHandler

#### `isAuthorized`

``` purescript
isAuthorized :: forall msg state. (Req -> state -> Effect (RestResult Authorized state)) -> StetsonHandler msg state -> StetsonHandler msg state
```

Add an isAuthorized callback to the provided StetsonHandler

#### `isConflict`

``` purescript
isConflict :: forall msg state. (Req -> state -> Effect (RestResult Boolean state)) -> StetsonHandler msg state -> StetsonHandler msg state
```

Add an isConflict callback to the provided StetsonHandler

#### `contentTypesAccepted`

``` purescript
contentTypesAccepted :: forall msg state. (Req -> state -> Effect (RestResult (List (Tuple2 String (AcceptHandler state))) state)) -> StetsonHandler msg state -> StetsonHandler msg state
```

Add a contentTypesAccepted callback to the provided StetsonHandler

#### `contentTypesProvided`

``` purescript
contentTypesProvided :: forall msg state. (Req -> state -> Effect (RestResult (List (Tuple2 String (ProvideHandler state))) state)) -> StetsonHandler msg state -> StetsonHandler msg state
```

Add a contentTypesProvided callback to the provided StetsonHandler

#### `deleteResource`

``` purescript
deleteResource :: forall msg state. (Req -> state -> Effect (RestResult Boolean state)) -> StetsonHandler msg state -> StetsonHandler msg state
```

Add a deleteResource callback to the provided StetsonHandler

#### `movedTemporarily`

``` purescript
movedTemporarily :: forall msg state. (Req -> state -> Effect (RestResult MovedResult state)) -> StetsonHandler msg state -> StetsonHandler msg state
```

Add a movedTemporarily callback to the provided StetsonHandler

#### `movedPermanently`

``` purescript
movedPermanently :: forall msg state. (Req -> state -> Effect (RestResult MovedResult state)) -> StetsonHandler msg state -> StetsonHandler msg state
```

Add a movedPermanently callback to the provided StetsonHandler

#### `serviceAvailable`

``` purescript
serviceAvailable :: forall msg state. (Req -> state -> Effect (RestResult Boolean state)) -> StetsonHandler msg state -> StetsonHandler msg state
```

Add a serviceAvailable callback to the provided StetsonHandler

#### `previouslyExisted`

``` purescript
previouslyExisted :: forall msg state. (Req -> state -> Effect (RestResult Boolean state)) -> StetsonHandler msg state -> StetsonHandler msg state
```

Add a previouslyExisted callback to the provided StetsonHandler

#### `switchHandler`

``` purescript
switchHandler :: forall reply state. CowboyHandler -> Req -> state -> Effect (RestResult reply state)
```

Switches to a different handler (probably cowboy_loop)

#### `forbidden`

``` purescript
forbidden :: forall msg state. (Req -> state -> Effect (RestResult Boolean state)) -> StetsonHandler msg state -> StetsonHandler msg state
```

Add a forbidden callback to the provided StetsonHandler

#### `terminate`

``` purescript
terminate :: forall msg state. (Foreign -> Req -> state -> Effect Unit) -> StetsonHandler msg state -> StetsonHandler msg state
```

Add a terminate callback to the provided StetsonHandler

#### `initResult`

``` purescript
initResult :: forall state. Req -> state -> Effect (InitResult state)
```

Create an init response for return from an InitHandler

#### `result`

``` purescript
result :: forall reply state. reply -> Req -> state -> Effect (RestResult reply state)
```

Create a rest response for return from a rest callback

#### `stop`

``` purescript
stop :: forall reply state. Req -> state -> Effect (RestResult reply state)
```

Create a rest stop response for return from a rest callback

#### `preHook`

``` purescript
preHook :: forall msg state. (forall state2. String -> Req -> state2 -> Effect Unit) -> StetsonHandler msg state -> StetsonHandler msg state
```

#### `preHook'`

``` purescript
preHook' :: forall msg state. (forall a state2. (String -> (Req -> state2 -> Effect a) -> (Req -> state2 -> Effect a))) -> StetsonHandler msg state -> StetsonHandler msg state
```

Add a hook in front of every call to a handler


