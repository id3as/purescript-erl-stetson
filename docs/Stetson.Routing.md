## Module Stetson.Routing

#### `GDispatch`

``` purescript
class GDispatch rep (r :: Row Type) | rep -> r where
  gDispatch :: Record r -> rep -> RouteHandler
```

##### Instances
``` purescript
(GDispatch a r, GDispatch b r) => GDispatch (Sum a b) r
(IsSymbol sym, Cons sym h rx r, GDispatchCtor c h) => GDispatch (Constructor sym c) r
```

#### `GDispatchCtor`

``` purescript
class GDispatchCtor rep f  where
  gDispatchC :: f -> rep -> RouteHandler
```

##### Instances
``` purescript
GDispatchCtor NoArguments (StetsonHandler x s)
GDispatchCtor NoArguments StaticAssetLocation
GDispatchCtor (Argument a) (a -> StetsonHandler x s)
GDispatchCtor (Argument (Array String)) StaticAssetLocation
GDispatchCtor (Argument (Array String)) (Array String -> StaticAssetLocation)
GDispatchCtor (Argument a) (a -> StaticAssetLocation)
(Generic a rep, GDispatch rep r) => GDispatchCtor (Argument a) (Record r)
(GDispatchCtor right b) => GDispatchCtor (Product (Argument a) right) (a -> b)
GDispatchCtor any CowboyRoutePlaceholder
```

#### `dummyHandler`

``` purescript
dummyHandler :: StetsonHandler Unit Unit
```


