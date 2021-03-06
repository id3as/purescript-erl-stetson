module Stetson.Test.Routes where

import Prelude hiding ((/))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Newtype (class Newtype, unwrap, wrap)
import Routing.Duplex (RouteDuplex', as, path)
import Routing.Duplex as RouteDuplex
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route
  = TestBarebones
  | TestFullyLoaded

derive instance genericRoute :: Generic Route _

instance showRoute :: Show Route where
  show = genericShow

asNewtype :: forall a. Newtype a String => RouteDuplex' String -> RouteDuplex' a
asNewtype = as unwrap (pure <<< wrap)

apiRoute :: RouteDuplex' Route
apiRoute =
  path ""
    $ sum
        { "TestBarebones": "api" / "testbarebones" / noArgs
        , "TestFullyLoaded": "api" / "testfullyloaded" / noArgs
        }

routeUrl :: Route -> String
routeUrl = RouteDuplex.print apiRoute

data NestedRoute
  = One Route
  | Two Route
  
derive instance genericNestedRoute :: Generic NestedRoute _

instance showNestedRoute :: Show NestedRoute where
  show = genericShow

apiRoute' :: RouteDuplex' Route
apiRoute' = sum
        { "TestBarebones": "api" / "testbarebones" / noArgs
        , "TestFullyLoaded": "api" / "testfullyloaded" / noArgs
        }

nestedApiRoute :: RouteDuplex' NestedRoute
nestedApiRoute =
  path ""
    $ sum
        { "One": "api" / "one" / apiRoute'
        , "Two": "api" / "two" / apiRoute'
        }
