module Stetson where

import Prelude

import Cowboy.Static as Static
import Data.Maybe (Maybe)
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Cowboy (ProtoEnv(..), ProtoOpt(..), TransOpt(..), env, protocolOpts)
import Erl.Cowboy as Cowboy
import Erl.Cowboy.Req (Req)
import Erl.Cowboy.Routes (Path)
import Erl.Cowboy.Routes as Routes
import Erl.Data.List (List, nil, reverse, singleton, (:))
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

data RestResult reply state = RestOk reply Req state
data InitResult state = InitOk Req state

type InitHandler state = Req -> Effect (InitResult state)
type AcceptHandler state = Req -> state -> Effect (RestResult Boolean state)
type ProvideHandler state = Req -> state -> Effect (RestResult String state)

type RestHandler state = {
    init :: Req -> Effect (InitResult state)
  , allowedMethods :: Maybe (Req -> state -> Effect (RestResult (List HttpMethod) state))
  , resourceExists :: Maybe (Req -> state -> Effect (RestResult Boolean state))
  , contentTypesAccepted :: Maybe (Req -> state -> Effect (RestResult (List (Tuple2 String (AcceptHandler state))) state))
  , contentTypesProvided :: Maybe (Req -> state -> Effect (RestResult (List (Tuple2 String (ProvideHandler state))) state))
  }

data HttpMethod = GET | POST | HEAD | OPTIONS | PUT | DELETE

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
type StetsonConfig = {
  bindPort :: Int
, bindAddress :: Tuple4 Int Int Int Int
, routes :: List ConfiguredRoute
  }

configure :: StetsonConfig
configure = 
  { bindPort : 8000
  , bindAddress : tuple4 0 0 0 0
  , routes : nil
  }

route :: forall state. String -> StetsonHandler state -> StetsonConfig -> StetsonConfig
route value (Rest handler) config@{ routes } =
  (config { routes = (Stetson { route: value
                              , moduleName: (nativeModuleName ModuleNames.stetsonRestHandler)
                              , args: unsafeCoerce handler
                              } : routes) })

static :: forall state. String -> StaticAssetLocation -> StetsonConfig -> StetsonConfig
static url (PrivDir app dir) config@{ routes } =
  (config { routes = Cowboy (Static.privDir (atom app) url dir) : routes })
static url (PrivFile app file) config@{ routes } =
  (config { routes = Cowboy (Static.privFile (atom app) url file) : routes })

cowboyRoutes :: List Path -> StetsonConfig -> StetsonConfig
cowboyRoutes newRoutes config@{ routes } = 
  (config { routes = (Cowboy <$> reverse newRoutes) <> routes })

port :: Int -> StetsonConfig -> StetsonConfig
port value config = 
  (config { bindPort = value })

bindTo :: Int -> Int -> Int -> Int -> StetsonConfig -> StetsonConfig
bindTo t1 t2 t3 t4 config = 
  (config { bindAddress = tuple4 t1 t2 t3 t4 })

startClear :: String -> StetsonConfig -> Effect Unit
startClear name config@{ bindAddress, bindPort } = do
  let paths = createRoute <$> reverse config.routes
      dispatch = Routes.compile $ singleton $ Routes.anyHost paths
      transOpts = Ip bindAddress : Port bindPort : nil
      protoOpts = protocolOpts $ Env (env (Dispatch dispatch : nil)) : nil
  _ <- Cowboy.startClear (atom name) transOpts protoOpts
  -- info "Started HTTP listener on port ~p." $ bindPort : nil
  pure unit


createRoute :: ConfiguredRoute -> Path
createRoute (Stetson { route, moduleName, args }) =
  Routes.path route moduleName (Routes.InitialState $ unsafeToForeign args)

createRoute (Cowboy path) = path


