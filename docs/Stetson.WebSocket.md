## Module Stetson.WebSocket

#### `handler`

``` purescript
handler :: forall msg state. InitHandler state -> StetsonHandler msg state
```

#### `init`

``` purescript
init :: forall msg state. WebSocketInitHandler msg state -> StetsonHandler msg state -> StetsonHandler msg state
```

#### `handle`

``` purescript
handle :: forall msg state. WebSocketHandleHandler msg state -> StetsonHandler msg state -> StetsonHandler msg state
```

#### `info`

``` purescript
info :: forall msg state. WebSocketInfoHandler msg state -> StetsonHandler msg state -> StetsonHandler msg state
```

#### `initResult`

``` purescript
initResult :: forall state. Req -> state -> Effect (InitResult state)
```

#### `terminate`

``` purescript
terminate :: forall msg state. (Foreign -> Req -> state -> Effect Unit) -> StetsonHandler msg state -> StetsonHandler msg state
```

Add a terminate callback to the provided StetsonHandler


### Re-exported from Effect.Class:

#### `liftEffect`

``` purescript
liftEffect :: forall m a. MonadEffect m => Effect a -> m a
```

### Re-exported from Erl.Process:

#### `self`

``` purescript
self :: forall x a. HasSelf x a => x (Process a)
```

