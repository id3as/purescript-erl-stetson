## Module Stetson.Test.Handlers

#### `State`

``` purescript
newtype State
  = State (Record ())
```

#### `HandlerState`

``` purescript
type HandlerState = { handler :: String, userData :: Maybe String }
```

#### `serverName`

``` purescript
serverName :: ServerName State Unit
```

#### `startLink`

``` purescript
startLink :: Boolean -> Effect StartLinkResult
```

#### `stopLink`

``` purescript
stopLink :: Effect Unit
```

#### `testStetsonConfig`

``` purescript
testStetsonConfig :: Init State Unit
```

#### `testStetsonConfig2`

``` purescript
testStetsonConfig2 :: Init State Unit
```

#### `bareBonesHandler`

``` purescript
bareBonesHandler :: StetsonHandler Unit HandlerState
```

#### `fullyLoadedHandler`

``` purescript
fullyLoadedHandler :: StetsonHandler Unit HandlerState
```

#### `test2`

``` purescript
test2 :: SimpleStetsonHandler HandlerState
```

#### `allBody`

``` purescript
allBody :: Req -> IOData -> Effect Binary
```

#### `restHandler`

``` purescript
restHandler :: forall responseType state. responseType -> Req -> state -> Effect (RestResult responseType state)
```

#### `cowboyRoutes`

``` purescript
cowboyRoutes :: List Path
```

#### `jsonWriter`

``` purescript
jsonWriter :: forall a. WriteForeign a => Tuple2 String (Req -> a -> (Effect (RestResult String a)))
```


