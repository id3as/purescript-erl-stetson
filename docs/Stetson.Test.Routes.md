## Module Stetson.Test.Routes

#### `Route`

``` purescript
data Route
  = TestBarebones
  | TestFullyLoaded
```

##### Instances
``` purescript
Generic Route _
Show Route
```

#### `asNewtype`

``` purescript
asNewtype :: forall a. Newtype a String => RouteDuplex' String -> RouteDuplex' a
```

#### `apiRoute`

``` purescript
apiRoute :: RouteDuplex' Route
```

#### `routeUrl`

``` purescript
routeUrl :: Route -> String
```


