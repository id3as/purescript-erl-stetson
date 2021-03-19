## Module Stetson.Loop

#### `self`

``` purescript
self :: forall msg. StateT (Process msg) Effect (Process msg)
```

#### `initResult`

``` purescript
initResult :: forall state. Req -> state -> Effect (InitResult state)
```

#### `init`

``` purescript
init :: forall msg state. LoopInitHandler msg state -> StetsonHandler msg state -> StetsonHandler msg state
```

#### `handler`

``` purescript
handler :: forall msg state. InitHandler state -> StetsonHandler msg state
```

#### `info`

``` purescript
info :: forall msg state. LoopInfoHandler msg state -> StetsonHandler msg state -> StetsonHandler msg state
```


### Re-exported from Control.Monad.Trans.Class:

#### `lift`

``` purescript
lift :: forall m a t. MonadTrans t => Monad m => m a -> t m a
```

