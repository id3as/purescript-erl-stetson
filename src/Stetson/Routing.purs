module Stetson.Routing where

import Prelude

import Data.Exists (mkExists)
import Data.Generic.Rep (Argument(..), Constructor(..), NoArguments(..), Product(..), Sum(..))
import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row as Row
import Record as Record
import Stetson.Rest as Rest
import Stetson.Types

class GDispatch rep (r :: # Type) | rep -> r where
  gDispatch :: { | r } -> rep -> RouteHandler

instance gDispatchSum ::
  ( GDispatch a r
  , GDispatch b r
  ) =>
  GDispatch (Sum a b) r where
  gDispatch r (Inl a) = gDispatch r a 
  gDispatch r (Inr b) = gDispatch r b 
    
instance gDispatchConstructor ::
  ( IsSymbol sym
  , Row.Cons sym h rx r
  , GDispatchCtor c h
  ) =>
  GDispatch (Constructor sym c) r where
    gDispatch r (Constructor rep) = gDispatchC handler rep 
      where
      handler = Record.get (SProxy :: SProxy sym) r

class GDispatchCtor rep f where
  gDispatchC :: f -> rep -> RouteHandler

instance gDispatchC0 :: GDispatchCtor NoArguments (StetsonHandler x s) where
  gDispatchC handler NoArguments = StetsonRoute (mkStetsonRoute handler)
instance gDispatchStatic :: GDispatchCtor NoArguments StaticAssetLocation where
  gDispatchC route NoArguments = StaticRoute [] route

instance gDispatchC1 :: GDispatchCtor (Argument a) (a -> (StetsonHandler x s)) where
  gDispatchC handler (Argument a) = StetsonRoute (mkStetsonRoute (handler a))
instance gDispatchStatic1 :: GDispatchCtor (Argument (Array String)) StaticAssetLocation where
  gDispatchC route (Argument a) = StaticRoute a route
else instance gDispatchStatic1Ignore :: GDispatchCtor (Argument a) (a -> StaticAssetLocation) where
  gDispatchC route (Argument a) = StaticRoute [] (route a)


instance gDispatchCN :: 
  ( GDispatchCtor right b
  ) => 
  GDispatchCtor (Product (Argument a) right) (a -> b) where
  gDispatchC handler (Product (Argument a) right) = gDispatchC (handler a) right

instance gDispatchCowboy :: GDispatchCtor any CowboyRoutePlaceholder where
  gDispatchC route anything = CowboyRouteFallthrough

dummyHandler :: StetsonHandler Unit Unit
dummyHandler = Rest.handler (\req -> pure $ Rest req unit)
