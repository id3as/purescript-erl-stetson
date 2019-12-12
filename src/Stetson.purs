-- | This is the entry point into the Stetson wrapper
-- | You'll want to call Stetson.configure and then follow the types..
module Stetson ( RestResult(..)
               , InitResult(..)
               , InitHandler
               , AcceptHandler
               , ProvideHandler
               , RestHandler
               , HttpMethod(..)
               , Authorized(..)
               , StetsonHandler(..)
               , StaticAssetLocation(..)
               , StetsonRoute
               , HandlerArgs
               , ConfiguredRoute(..)
               , StetsonConfig
               , configure
               , route
               , static
               , cowboyRoutes
               , port
               , bindTo
               , streamHandlers
               , middlewares
               , startClear
  ) where

import Prelude

import Cowboy.Static as Static
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Cowboy (ProtoEnv(..), ProtoOpt(..), TransOpt(..), env, protocolOpts)
import Erl.Cowboy as Cowboy
import Erl.Cowboy.Handlers.Rest (MovedResult)
import Erl.Cowboy.Req (Req)
import Erl.Cowboy.Routes (Path)
import Erl.Cowboy.Routes as Routes
import Erl.Data.List (List, nil, reverse, singleton, (:))
import Erl.Data.List as List
import Erl.Data.Tuple (Tuple2, Tuple4, tuple4)
import Erl.ModuleName (NativeModuleName, nativeModuleName)
import Foreign (unsafeToForeign)
import Stetson.ModuleNames as ModuleNames
import Unsafe.Coerce (unsafeCoerce)

foreign import data HandlerArgs :: Type

-- Thoughts here are to mirror the cowboy API as much as possible whilst
--   - not tying ourselves to it in application code
--   - providing more idiomatic types for composition/etc
-- It's a bit of a faff, but it means if Cowboy decide to get rid of the undocumented behaviour we're abusing
-- We can switch away from the underlying engine, write our own or use cowlib directly (or just re-write Cowboy in Purescript)
-- There are a lot of reasons why we would want a properly idiomatic http server in PS, but hopefully somebody will do that for us and replace the need  for this entirely
-- Now there's a fun job for a long weekend
-- The exception is cowboy_req, as that's pretty universal across handlers and isn't too ridiculous to talk to directly
-- We could go with our own req module, but that would probably just end up being 1:1 to cowboy req anyway so who needs that extra work


-- | The return type of most of the callbacks invoked as part of the REST workflow
data RestResult reply state = RestOk reply Req state

-- | The return type of the 'init' callback in the REST workflow
data InitResult state = InitOk Req state

-- | The callback invoked to kick off the REST workflow
type InitHandler state = Req -> Effect (InitResult state)

-- | A callback invoked to 'accept' a specific content type
type AcceptHandler state = Req -> state -> Effect (RestResult Boolean state)

-- | A callback invoked to 'provide' a specific content type
type ProvideHandler state = Req -> state -> Effect (RestResult String state)

-- | A builder containing the complete set of callbacks during the rest workflow for a specific handler
type RestHandler state = {
    init :: Req -> Effect (InitResult state)
  , allowedMethods :: Maybe (Req -> state -> Effect (RestResult (List HttpMethod) state))
  , resourceExists :: Maybe (Req -> state -> Effect (RestResult Boolean state))
  , contentTypesAccepted :: Maybe (Req -> state -> Effect (RestResult (List (Tuple2 String (AcceptHandler state))) state))
  , contentTypesProvided :: Maybe (Req -> state -> Effect (RestResult (List (Tuple2 String (ProvideHandler state))) state))
  , deleteResource :: Maybe (Req -> state -> Effect (RestResult Boolean state))
  , isAuthorized :: Maybe (Req -> state -> Effect (RestResult Authorized state))
  , isConflict :: Maybe (Req -> state -> Effect (RestResult Boolean state))
  , movedTemporarily :: Maybe (Req -> state -> Effect (RestResult MovedResult state))
  , movedPermanently :: Maybe (Req -> state -> Effect (RestResult MovedResult state))
  , serviceAvailable :: Maybe (Req -> state -> Effect (RestResult Boolean state))
  , previouslyExisted :: Maybe (Req -> state -> Effect (RestResult Boolean state))
  , forbidden :: Maybe (Req -> state -> Effect (RestResult Boolean state))
  }

-- | or is it a verb
data HttpMethod = GET | POST | HEAD | OPTIONS | PUT | DELETE

-- | Return type of the isAuthorized callback
data Authorized = Authorized | NotAuthorized String

instance showHttpMethod :: Show HttpMethod where
  show method = case method of
                     GET -> "GET"
                     POST -> "POST"
                     HEAD -> "HEAD"
                     OPTIONS -> "OPTIONS"
                     PUT -> "PUT"
                     DELETE -> "DELETE"

data StetsonHandler state = Rest (RestHandler state)
data StaticAssetLocation = PrivDir String String
                         | PrivFile String String

type StetsonRoute =
  { route :: String
  , moduleName :: NativeModuleName
  , args :: HandlerArgs
  }

data ConfiguredRoute = Stetson StetsonRoute | Cowboy Path

-- Probably want to make this look a bit more like Cowboy's config internally
-- Lists of maps or tuples or whatever the hell cowboy is using in whatever version we're bound to
type StetsonConfig =
  { bindPort :: Int
  , bindAddress :: Tuple4 Int Int Int Int
  , streamHandlers :: Maybe (List NativeModuleName)
  , middlewares :: Maybe (List NativeModuleName)
  , routes :: List ConfiguredRoute
  }

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
route :: forall state. String -> StetsonHandler state -> StetsonConfig -> StetsonConfig
route value (Rest handler) config@{ routes } =
  (config { routes = (Stetson { route: value
                              , moduleName: (nativeModuleName ModuleNames.stetsonRestHandler)
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
