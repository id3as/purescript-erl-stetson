-- | This is the entry point into the Stetson wrapper
-- | You'll want to call Stetson.configure and then follow the types..
module Stetson
  ( configure
  , cowboyRoutes
  , routes
  , routes2
  , port
  , bindTo
  , streamHandlers
  , middlewares
  , tcpOptions
  , tlsOptions
  , startClear
  , startTls
  , stop
  , module Stetson.Types
  ) where

import Prelude

import Control.Alt ((<|>))
import Cowboy.Static as CowboyStatic
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, NoArguments, from)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Erl.Atom (Atom, atom)
import Erl.Cowboy (ProtocolOpts)
import Erl.Cowboy as Cowboy
import Erl.Cowboy.Req (Req)
import Erl.Cowboy.Routes (Path)
import Erl.Cowboy.Routes as Routes
import Erl.Data.List (List, nil, null, reverse, singleton, (:))
import Erl.Data.List as List
import Erl.Data.Map (Map)
import Erl.Data.Map as Map
import Erl.Data.Tuple (tuple2, tuple3)
import Erl.Kernel.Inet (Ip4Address, IpAddress(..), Port(..), SocketAddress(..), ip4Any)
import Erl.Kernel.Tcp as Tcp
import Erl.ModuleName (NativeModuleName(..), nativeModuleName)
import Erl.Ssl as Ssl
import Foreign (Foreign, unsafeToForeign)
import Routing.Duplex (RouteDuplex, root)
import Routing.Duplex.Generic (noArgs)
import RoutingDuplexMiddleware (UnmatchedHandler(..))
import RoutingDuplexMiddleware as RoutingMiddleware
import Stetson.ModuleNames as ModuleNames
import Stetson.Routing (class GDispatch, gDispatch)
import Stetson.Routing as Routing
import Stetson.Types (AcceptHandler, Authorized(..), CowboyHandler(..), HandlerArgs, HttpMethod(..), InitHandler, InitResult(..), LoopCallResult(..), ProvideHandler, RestResult(..), RouteConfig, RouteHandler(..), SimpleStetsonHandler, StaticAssetLocation(..), StetsonHandler(..), WebSocketCallResult(..), WebSocketHandleHandler, WebSocketInfoHandler, WebSocketInitHandler, StetsonConfig, emptyHandler, mkStetsonRoute, runStetsonRoute)
import Unsafe.Coerce (unsafeCoerce)

-- | Creates a blank stetson config with default settings and no routes
configure :: StetsonConfig NoArguments NoArguments
configure =
  { bindPort: (Port 8000)
  , bindAddress: ip4Any
  , streamHandlers: Nothing
  , middlewares:
      Just
        ( mod "routingDuplexMiddleware@ps"
            : mod "cowboy_handler"
            : nil
        )
  , tcpOptions: Nothing
  , tlsOptions: Nothing
  , cowboyRoutes: nil
  , routes:
      { routing: root noArgs
      , dispatch: \_ -> StetsonRoute $ mkStetsonRoute Routing.dummyHandler
      }
  }
  where
  mod :: String -> NativeModuleName
  mod s = NativeModuleName (atom s)

routes ::
  forall t' a' t a rep r.
  Generic a rep =>
  GDispatch rep r =>
  RouteDuplex t a -> { | r } -> StetsonConfig t' a' -> StetsonConfig t a
routes routing d config = config { routes = { routing: routing, dispatch: dispatchTable d } }
  where
  dispatchTable ::
    { | r } -> a -> RouteHandler
  dispatchTable r a = gDispatch r (from a)

routes2 ::
  forall t a rep r.
  Generic a rep =>
  GDispatch rep r =>
  RouteDuplex t a -> { | r } -> RouteConfig t a
routes2 routing d = { routing: routing, dispatch: dispatchTable d }
  where
  dispatchTable ::
    { | r } -> a -> RouteHandler
  dispatchTable r a = gDispatch r (from a)

-- | Introduce a list of native Erlang cowboy handlers to this config
cowboyRoutes :: forall t a. List Path -> StetsonConfig t a -> StetsonConfig t a
cowboyRoutes newRoutes config@{ cowboyRoutes: existingRoutes } = (config { cowboyRoutes = reverse newRoutes <> existingRoutes })

-- | Set the port that this http listener will listen to
port :: forall t a. Port -> StetsonConfig t a -> StetsonConfig t a
port value config = (config { bindPort = value })

-- | Set the IP that this http listener will bind to (default: 0.0.0.0)
bindTo :: forall t a. Ip4Address -> StetsonConfig t a -> StetsonConfig t a
bindTo ip4Address config = (config { bindAddress = ip4Address })

-- | Supply a list of modules to act as native stream handlers in cowboy
streamHandlers :: forall t a. List NativeModuleName -> StetsonConfig t a -> StetsonConfig t a
streamHandlers handlers config = (config { streamHandlers = Just handlers })

-- | Supply a list of modules to act as native middlewares in cowboy
middlewares :: forall t a. List NativeModuleName -> StetsonConfig t a -> StetsonConfig t a
middlewares mws config = (config { middlewares = Just mws })

-- | Supply tcp transport options for cowboy/ranch
tcpOptions :: forall t a. Record Tcp.ListenOptions -> StetsonConfig t a -> StetsonConfig t a
tcpOptions opts config = config { tcpOptions = Just opts }

-- | Supply tls/ssl transport options for cowboy/ranch
tlsOptions :: forall t a. Record Ssl.ListenOptions -> StetsonConfig t a -> StetsonConfig t a
tlsOptions opts config = config { tlsOptions = Just opts }

-- | Start the listener with the specified name
startClear :: forall t a. String -> StetsonConfig t a -> Effect (Either Foreign Unit)
startClear name config@{ bindAddress, bindPort, tcpOptions: tcpOptions_ } = do
  let
    listenOpts = fromMaybe (Tcp.listenOptions {}) tcpOptions_
    opts =
      Cowboy.defaultOptions
        { socket_opts =
          Just
            $ listenOpts
                { port = listenOpts.port <|> Just bindPort
                , ip = listenOpts.ip <|> (Just $ IpAddress $ Ip4 bindAddress)
                }
        }
  Cowboy.startClear (atom name) opts (protoOpts config)

-- | Start the TLS listener with the specified name
startTls :: forall t a. String -> StetsonConfig t a -> Effect (Either Foreign Unit)
startTls name config@{ bindAddress, bindPort, tlsOptions: tlsOptions_ } = do
  let
    listenOpts = fromMaybe Ssl.defaultListenOptions tlsOptions_
    opts =
      Cowboy.defaultOptions
        { socket_opts =
          Just
            $ listenOpts
                { port = listenOpts.port <|> Just bindPort
                , ip = listenOpts.ip <|> (Just $ IpAddress $ Ip4 bindAddress)
                }
        }
  Cowboy.startTls (atom name) opts (protoOpts config)

protoOpts :: forall t a. StetsonConfig t a -> ProtocolOpts
protoOpts config@{ streamHandlers: streamHandlers_, middlewares: middlewares_, cowboyRoutes: cowboyRoutes' } =
  { env:
      Just
        ( Map.empty
            # Cowboy.dispatch dispatch
            # RoutingMiddleware.routes config.routes.routing { dispatch: mapping, unmatched }
        )
  , streamHandlers: streamHandlers_
  , middlewares: middlewares_
  }

  where
  dispatch = Routes.compile $ singleton $ Routes.anyHost $ reverse cowboyRoutes'
  unmatched _ =
    if null cowboyRoutes' then
      Default
    else
      CowboyRouterFallback

  mapping req route = case config.routes.dispatch route of
    StetsonRoute inner -> runStetsonRoute (mapRoute req) inner
    StaticRoute pathSegments (PrivDir app dir) ->
      let
        req' = Map.insert (atom "path_info") (List.fromFoldable pathSegments) (unsafeCoerce req :: Map Atom (List String))
      in
        tuple2 (unsafeCoerce req' :: Req)
          $ Right
              { mod: CowboyStatic.moduleName
              , args: unsafeToForeign $ tuple3 (atom "priv_dir") (atom app) dir
              }
    StaticRoute _ (PrivFile app file) ->
      tuple2 req
        $ Right
            { mod: CowboyStatic.moduleName
            , args: unsafeToForeign $ tuple3 (atom "priv_file") (atom app) file
            }
    CowboyRouteFallthrough -> tuple2 req $ Left CowboyRouterFallback

  mapRoute :: forall b c. _ -> StetsonHandler b c -> _
  mapRoute req = case _ of
    (StetsonHandler handler) ->
      tuple2 req
        $ Right
            { mod: nativeModuleName ModuleNames.stetsonHandlerProxy
            , args: unsafeToForeign { handler, innerState: unit, acceptHandlers: nil, provideHandlers: nil }
            }

stop :: String -> Effect Unit
stop name = Cowboy.stopListener (atom name)
