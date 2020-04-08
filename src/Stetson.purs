-- | This is the entry point into the Stetson wrapper
-- | You'll want to call Stetson.configure and then follow the types..
module Stetson
  ( configure
  , cowboyRoutes
  , routes
  , port
  , bindTo
  , streamHandlers
  , middlewares
  , startClear
  , module Stetson.Types
  )
  where

import Prelude

import Cowboy.Static as CowboyStatic
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, NoArguments, from)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Cowboy (ProtoOpt(..), TransOpt(..), protocolOpts)
import Erl.Cowboy as Cowboy
import Erl.Cowboy.Req (Req)
import Erl.Cowboy.Routes (Path)
import Erl.Cowboy.Routes as Routes
import Erl.Data.List (List, nil, null, reverse, singleton, (:))
import Erl.Data.List as List
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Data.Tuple (tuple2, tuple3, tuple4)
import Erl.ModuleName (NativeModuleName(..), nativeModuleName)
import Foreign (unsafeToForeign)
import Routing.Duplex (RouteDuplex', root)
import Routing.Duplex.Generic (noArgs)
import RoutingDuplexMiddleware (UnmatchedHandler(..))
import RoutingDuplexMiddleware as RoutingMiddleware
import Stetson.ModuleNames as ModuleNames
import Stetson.Routing (class GDispatch, gDispatch)
import Stetson.Routing as Routing
import Stetson.Types (AcceptHandler, Authorized(..), HandlerArgs, HttpMethod(..), InitHandler, InitResult(..), InnerStetsonHandler(..), ProvideHandler, ReceivingStetsonHandler, RestHandler, RestResult(..), RouteHandler(..), StaticAssetLocation(..), StetsonConfig, StetsonHandler, WebSocketCallResult(..), WebSocketHandleHandler, WebSocketHandler, WebSocketInfoHandler, WebSocketInitHandler, WebSocketMessageRouter, runStetsonRoute, mkStetsonRoute)
import Unsafe.Coerce (unsafeCoerce)

-- | Creates a blank stetson config with default settings and no routes
configure :: StetsonConfig NoArguments
configure =
  { bindPort : 8000
  , bindAddress : tuple4 0 0 0 0
  , streamHandlers : Nothing
  , middlewares : Just ( mod "routingDuplexMiddleware@ps"
                       : mod "cowboy_handler"
                       : nil
                       )
  , cowboyRoutes : nil
  , routing : root noArgs
  , dispatch : \_ -> StetsonRoute $ mkStetsonRoute Routing.dummyHandler
  }
  where
  mod :: String -> NativeModuleName
  mod s = NativeModuleName (atom s)

routes :: forall a b rep r.
  Generic a rep =>
  GDispatch rep r =>
  RouteDuplex' a -> { | r } -> StetsonConfig b -> StetsonConfig a
routes routing d config = config { routing = routing, dispatch = dispatchTable d }
  where
  dispatchTable ::
    { | r } -> a -> RouteHandler
  dispatchTable r a = gDispatch r (from a)
-- route value (WebSocket handler) config@{ routes } =
--   (config { routes = (Stetson { route: value
--                               , moduleName: (nativeModuleName ModuleNames.stetsonWebSocketHandler)
--                               , args: unsafeCoerce handler
--                               } : routes) })


-- | Introduce a list of native Erlang cowboy handlers to this config
cowboyRoutes :: forall a. List Path -> StetsonConfig a -> StetsonConfig a
cowboyRoutes newRoutes config@{ cowboyRoutes: existingRoutes } =
  (config { cowboyRoutes = reverse newRoutes <> existingRoutes })

-- | Set the port that this http listener will listen to
port :: forall a. Int -> StetsonConfig a -> StetsonConfig a
port value config =
  (config { bindPort = value })

-- | Set the IP that this http listener will bind to (default: 0.0.0.0)
bindTo :: forall a. Int -> Int -> Int -> Int -> StetsonConfig a -> StetsonConfig a
bindTo t1 t2 t3 t4 config =
  (config { bindAddress = tuple4 t1 t2 t3 t4 })

-- | Supply a list of modules to act as native stream handlers in cowboy
streamHandlers :: forall a. List NativeModuleName -> StetsonConfig a -> StetsonConfig a
streamHandlers handlers config =
  (config { streamHandlers = Just handlers })

-- | Supply a list of modules to act as native middlewares in cowboy
middlewares :: forall a. List NativeModuleName -> StetsonConfig a -> StetsonConfig a
middlewares mws config =
  (config { middlewares = Just mws })

-- | Start the listener with the specified name
startClear :: forall a. String -> StetsonConfig a -> Effect Unit
startClear name config@{ bindAddress, bindPort, streamHandlers: streamHandlers_, middlewares: middlewares_, cowboyRoutes: cowboyRoutes' } = do
  let dispatch = Routes.compile $ singleton $ Routes.anyHost $ reverse cowboyRoutes'
      transOpts = Ip bindAddress : Port bindPort : nil
      protoOpts = protocolOpts $
        singleton (Env (Map.empty # Cowboy.dispatch dispatch
                                  # RoutingMiddleware.routes config.routing { dispatch: mapping, unmatched }
                        ))
        <> List.fromFoldable (StreamHandlers <$> streamHandlers_)
        <> List.fromFoldable (Middlewares <$> middlewares_)
  _ <- Cowboy.startClear (atom name) transOpts protoOpts
  pure unit
  where
  unmatched _ = if null cowboyRoutes'
                then Default
                else CowboyRouterFallback

  mapping req route =
    case config.dispatch route of
      StetsonRoute inner -> runStetsonRoute (mapRoute req) inner

      StaticRoute pathSegments (PrivDir app dir) ->
        let req' = Map.insert (atom "path_info") (List.fromFoldable pathSegments) (unsafeCoerce req :: Map Atom (List String))
        in tuple2 (unsafeCoerce req' :: Req) $ Right
        { mod: CowboyStatic.moduleName
        , args: unsafeToForeign $ tuple3 (atom "priv_dir") (atom app) dir
        }
      StaticRoute _ (PrivFile app file) -> tuple2 req $ Right
        { mod: CowboyStatic.moduleName
        , args: unsafeToForeign $ tuple3 (atom "priv_file") (atom app) file
        }
      CowboyRouteFallthrough -> tuple2 req $ Left CowboyRouterFallback

  mapRoute :: forall b c. _ -> InnerStetsonHandler b c -> _
  mapRoute req = case _ of
    Rest handler -> tuple2 req $ Right
        { mod: nativeModuleName ModuleNames.stetsonRestHandler
        , args: unsafeToForeign handler
        }
    WebSocket handler -> tuple2 req $ Right
        { mod: nativeModuleName ModuleNames.stetsonWebSocketHandler
        , args: unsafeToForeign handler
        }
