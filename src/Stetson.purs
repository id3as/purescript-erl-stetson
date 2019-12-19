-- | This is the entry point into the Stetson wrapper
-- | You'll want to call Stetson.configure and then follow the types..
module Stetson ( configure
               , route
               , static
               , cowboyRoutes
               , port
               , bindTo
               , streamHandlers
               , middlewares
               , startClear
               , module Stetson.Types
  ) where

import Prelude 
import Stetson.Types (AcceptHandler, Authorized(..), ConfiguredRoute(..), HandlerArgs, HttpMethod(..), InitHandler, InitResult(..), InnerStetsonHandler(..), ProvideHandler, ReceivingStetsonHandler, RestHandler, RestResult(..), StaticAssetLocation(..), StetsonConfig, StetsonHandler, StetsonRoute, WebSocketCallResult(..), WebSocketHandleHandler, WebSocketHandler, WebSocketInfoHandler, WebSocketInitHandler, WebSocketMessageRouter)

import Cowboy.Static as Static
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Cowboy (ProtoEnv(..), ProtoOpt(..), TransOpt(..), env, protocolOpts)
import Erl.Cowboy as Cowboy
import Erl.Cowboy.Routes (Path)
import Erl.Cowboy.Routes as Routes
import Erl.Data.List (List, nil, reverse, singleton, (:))
import Erl.Data.List as List
import Erl.Data.Tuple (tuple4)
import Erl.ModuleName (NativeModuleName, nativeModuleName)
import Foreign (unsafeToForeign)
import Stetson.ModuleNames as ModuleNames
import Unsafe.Coerce (unsafeCoerce)


-- | Creates a blank stetson config with default settings and no routes
configure :: StetsonConfig
configure = 
  { bindPort : 8000
  , bindAddress : tuple4 0 0 0 0
  , streamHandlers : Nothing
  , middlewares : Nothing
  , routes : nil
  }

-- | Add a route to a StetsonConfig
-- | value: The path this route will handle (this takes the same format as cowboy routes)
-- | handler: The handler that will take care of this request
-- | config: The config to add this route to
-- | ```purescript
-- | let newConfig = Stetson.route "/items/:id" myHandler config
-- | ```
route :: forall msg state. String -> InnerStetsonHandler msg state -> StetsonConfig -> StetsonConfig
route value (Rest handler) config@{ routes } =
  (config { routes = (Stetson { route: value
                              , moduleName: (nativeModuleName ModuleNames.stetsonRestHandler)
                              , args: unsafeCoerce handler
                              } : routes) })

route value (WebSocket handler) config@{ routes } =
  (config { routes = (Stetson { route: value
                              , moduleName: (nativeModuleName ModuleNames.stetsonWebSocketHandler)
                              , args: unsafeCoerce handler
                              } : routes) })

-- | Add a static route handler to a StetsonConfig
-- | This can either be a file or a directory to serve a file or files from
static :: String -> StaticAssetLocation -> StetsonConfig -> StetsonConfig
static url (PrivDir app dir) config@{ routes } =
  (config { routes = Cowboy (Static.privDir (atom app) url dir) : routes })
static url (PrivFile app file) config@{ routes } =
  (config { routes = Cowboy (Static.privFile (atom app) url file) : routes })

-- | Introduce a list of native Erlang cowboy handlers to this config
cowboyRoutes :: List Path -> StetsonConfig -> StetsonConfig
cowboyRoutes newRoutes config@{ routes } = 
  (config { routes = (Cowboy <$> reverse newRoutes) <> routes })

-- | Set the port that this http listener will listen to
port :: Int -> StetsonConfig -> StetsonConfig
port value config = 
  (config { bindPort = value })

-- | Set the IP that this http listener will bind to (default: 0.0.0.0)
bindTo :: Int -> Int -> Int -> Int -> StetsonConfig -> StetsonConfig
bindTo t1 t2 t3 t4 config = 
  (config { bindAddress = tuple4 t1 t2 t3 t4 })

-- | Supply a list of modules to act as native stream handlers in cowboy
streamHandlers :: List NativeModuleName -> StetsonConfig -> StetsonConfig
streamHandlers handlers config =
  (config { streamHandlers = Just handlers })

-- | Supply a list of modules to act as native middlewares in cowboy
middlewares :: List NativeModuleName -> StetsonConfig -> StetsonConfig
middlewares mws config =
  (config { middlewares = Just mws })

-- | Start the listener with the specified name
startClear :: String -> StetsonConfig -> Effect Unit
startClear name config@{ bindAddress, bindPort, streamHandlers: streamHandlers_, middlewares: middlewares_ } = do
  let paths = createRoute <$> reverse config.routes
      dispatch = Routes.compile $ singleton $ Routes.anyHost paths
      transOpts = Ip bindAddress : Port bindPort : nil
      protoOpts = protocolOpts $ 
        Env (env (Dispatch dispatch : nil)) : nil 
        <> List.fromFoldable (StreamHandlers <$> streamHandlers_)
        <> List.fromFoldable (Middlewares <$> middlewares_)
  _ <- Cowboy.startClear (atom name) transOpts protoOpts
  pure unit


createRoute :: ConfiguredRoute -> Path
createRoute (Stetson { route: route_, moduleName, args }) =
  Routes.path route_ moduleName (Routes.InitialState $ unsafeToForeign args)

createRoute (Cowboy path) = path


