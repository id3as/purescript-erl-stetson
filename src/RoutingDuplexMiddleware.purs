module RoutingDuplexMiddleware (ModuleInfo, routes, execute, ExecuteResult, UnmatchedHandler(..), Config) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import Erl.Atom (Atom, atom)
import Erl.Cowboy (Env)
import Erl.Cowboy.Req (Req, StatusCode(..))
import Erl.Cowboy.Req as Req
import Erl.Data.Map as Map
import Erl.Data.Tuple (Tuple2, tuple2, tuple3, uncurry2)
import Erl.ModuleName (NativeModuleName)
import Foreign (Foreign, unsafeFromForeign, unsafeToForeign)
import Foreign as Foreign
import Routing.Duplex (RouteDuplex', parse)
import Routing.Duplex.Parser (RouteError)
import Unsafe.Coerce (unsafeCoerce)

routesKey :: Atom
routesKey = atom "routes"

data UnmatchedHandler = CowboyRouterFallback | Default | DefaultHandler ModuleInfo

type Config a =
  { dispatch :: Req -> a -> Tuple2 Req (Either UnmatchedHandler ModuleInfo)
  , unmatched :: RouteError -> UnmatchedHandler
  }

type ModuleInfo = { mod :: NativeModuleName, args :: Foreign }

routes :: forall a. RouteDuplex' a -> Config a -> Env -> Env
routes r config env = Map.insert routesKey (Foreign.unsafeToForeign (Routes r config))  env

data Routes a = Routes (RouteDuplex' a) (Config a)

foreign import cowboyRouterExecute :: Req -> Env -> Effect ExecuteResult

foreign import data ExecuteResult :: Type

okResult :: Req -> Env -> ExecuteResult
okResult req env = unsafeCoerce $ tuple3 (atom "ok") req env

stopResult :: Req -> ExecuteResult
stopResult req = unsafeCoerce $ tuple2 (atom "stop") req

execute :: Req -> Env -> ExecuteResult
execute req env = unsafePerformEffect $ do
  case Map.lookup routesKey env of
    Nothing ->
      throw "No routes"
    Just routesConfig -> do
      let Routes routes { dispatch, unmatched } = unsafeFromForeign routesConfig
      case parse routes (Req.path req) of
        Right parsedRoute -> do
          flip uncurry2 (dispatch req parsedRoute) \req' dispatched ->
            case dispatched of
              Left unmatched -> handleUnmatched unmatched
              Right matched ->
                let env' = updateEnv matched env
                in pure $ okResult req' env'
        Left err -> handleUnmatched $ unmatched err
  where
    handleUnmatched =
      case _ of
        Default -> do
          req' <- Req.replyStatus (StatusCode 404) req
          pure $ stopResult req'
        DefaultHandler handler -> 
          let env' = updateEnv handler env
          in pure $ okResult req env'
        CowboyRouterFallback ->
          cowboyRouterExecute req env

    updateEnv { mod, args } env = 
      Map.insert (atom "handler") (unsafeToForeign mod) 
      $ Map.insert (atom "handler_opts") args
      $ env