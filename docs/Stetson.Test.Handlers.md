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

#### `Stop`

``` purescript
data Stop
  = Stop
```

#### `Cont`

``` purescript
data Cont
  = Cont
```

#### `Msg`

``` purescript
type Msg = Unit
```

#### `ServerConfig`

``` purescript
data ServerConfig
  = NewStyle
  | OldStyle
  | NestedRoutes
```

#### `serverName`

``` purescript
serverName :: RegistryName (ServerType Cont Stop Msg State)
```

#### `startLink`

``` purescript
startLink :: ServerConfig -> Effect (StartLinkResult (ServerPid Cont Stop Msg State))
```

#### `stopLink`

``` purescript
stopLink :: Effect Unit
```

#### `routes`

``` purescript
routes :: { "TestBarebones" :: StetsonHandler Unit { handler :: String, userData :: Maybe String }, "TestFullyLoaded" :: StetsonHandler Unit { handler :: String, userData :: Maybe String } }
```

#### `testStetsonConfig`

``` purescript
testStetsonConfig :: InitFn Cont Stop Msg State
```

#### `testStetsonConfig2`

``` purescript
testStetsonConfig2 :: InitFn Cont Stop Msg State
```

#### `testStetsonConfigNested`

``` purescript
testStetsonConfigNested :: InitFn Cont Stop Msg State
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
jsonWriter :: forall a. WriteForeign a => Tuple2 String (Req -> a -> (Effect (RestResult IOData a)))
```


