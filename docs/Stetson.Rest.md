## Module Stetson.Rest

This module contains the functions necessary to define a rest handler for a route in Stetson/Cowboy
This maps pretty much 1-1 onto https://ninenines.eu/docs/en/cowboy/2.5/guide/rest_handlers/#_callbacks
Although only the handlers that we have needed so far are defined - feel free to send pull requests that add the ones you need

#### `handler`

``` purescript
handler :: forall state. InitHandler state -> RestHandler state
```

Create a cowboy REST handler with the provided Init handler and no callbacks defined

#### `allowedMethods`

``` purescript
allowedMethods :: forall state. (Req -> state -> Effect (RestResult (List HttpMethod) state)) -> RestHandler state -> RestHandler state
```

Add an allowedMethods callback to the provided RestHandler

#### `resourceExists`

``` purescript
resourceExists :: forall state. (Req -> state -> Effect (RestResult Boolean state)) -> RestHandler state -> RestHandler state
```

Add a resourceExists callback to the provided RestHandler

#### `isAuthorized`

``` purescript
isAuthorized :: forall state. (Req -> state -> Effect (RestResult Authorized state)) -> RestHandler state -> RestHandler state
```

Add an isAuthorized callback to the provided RestHandler

#### `contentTypesAccepted`

``` purescript
contentTypesAccepted :: forall state. (Req -> state -> Effect (RestResult (List (Tuple2 String (AcceptHandler state))) state)) -> RestHandler state -> RestHandler state
```

Add a contentTypesAccepted callback to the provided RestHandler

#### `contentTypesProvided`

``` purescript
contentTypesProvided :: forall state. (Req -> state -> Effect (RestResult (List (Tuple2 String (ProvideHandler state))) state)) -> RestHandler state -> RestHandler state
```

Add a contentTypesProvided callback to the provided RestHandler

#### `deleteResource`

``` purescript
deleteResource :: forall state. (Req -> state -> Effect (RestResult Boolean state)) -> RestHandler state -> RestHandler state
```

Add a deleteResource callback to the provided RestHandler

#### `movedTemporarily`

``` purescript
movedTemporarily :: forall state. (Req -> state -> Effect (RestResult MovedResult state)) -> RestHandler state -> RestHandler state
```

Add a movedTemporarily callback to the provided RestHandler

#### `movedPermanently`

``` purescript
movedPermanently :: forall state. (Req -> state -> Effect (RestResult MovedResult state)) -> RestHandler state -> RestHandler state
```

Add a movedPermanently callback to the provided RestHandler

#### `serviceAvailable`

``` purescript
serviceAvailable :: forall state. (Req -> state -> Effect (RestResult Boolean state)) -> RestHandler state -> RestHandler state
```

Add a serviceAvailable callback to the provided RestHandler

#### `previouslyExisted`

``` purescript
previouslyExisted :: forall state. (Req -> state -> Effect (RestResult Boolean state)) -> RestHandler state -> RestHandler state
```

Add a previouslyExisted callback to the provided RestHandler

#### `forbidden`

``` purescript
forbidden :: forall state. (Req -> state -> Effect (RestResult Boolean state)) -> RestHandler state -> RestHandler state
```

Add a forbidden callback to the provided RestHandler

#### `initResult`

``` purescript
initResult :: forall state. Req -> state -> Effect (InitResult state)
```

Create an init response for return from an InitHandler

#### `result`

``` purescript
result :: forall reply state. reply -> Req -> state -> Effect (RestResult reply state)
```

Create an rest response for return from a rest callback

#### `yeeha`

``` purescript
yeeha :: forall state. RestHandler state -> StetsonHandler state
```

Finish defining this rest handler, yeehaaw


