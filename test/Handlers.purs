module Stetson.Test.Handlers where

import Prelude

import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Atom (atom)
import Erl.Cowboy.Handlers.Rest (notMoved)
import Erl.Cowboy.Req (ReadBodyResult(..), Req, readBody, setBody)
import Erl.Cowboy.Routes (InitialState(..), Path(..), matchSpec)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData, fromBinary, toBinary, fromString)
import Erl.Data.List (List, (:), nil)
import Erl.Data.Tuple (Tuple2, tuple2, tuple3)
import Erl.Kernel.Inet (Port(..), ip4Any)
import Erl.ModuleName (NativeModuleName(..))
import Foreign (unsafeToForeign)
import Pinto (RegistryName(..), StartLinkResult)
import Pinto.GenServer as GS
import Pinto.Types (RegistryReference(..))
import Simple.JSON (class WriteForeign, readJSON, writeJSON)
import Stetson (Authorized(..), RestResult, SimpleStetsonHandler, StetsonHandler, acceptFailure, acceptSuccess)
import Stetson as Stetson
import Stetson.Rest as Rest
import Stetson.Test.Routes as TestRoutes
import Stetson.Types (routeHandler)
import Unsafe.Coerce (unsafeCoerce)

newtype State
  = State {}

type HandlerState
  = { handler :: String
    , userData :: Maybe String
    }

data Stop
  = Stop

data Cont
  = Cont

type Msg
  = Unit

data ServerConfig = NewStyle | OldStyle | NestedRoutes

serverName :: RegistryName (GS.ServerType Cont Stop Msg State)
serverName = Local $ atom "test_server"

startLink :: ServerConfig -> Effect (StartLinkResult (GS.ServerPid Cont Stop Msg State))
startLink = case _ of
  OldStyle -> GS.startLink $ (GS.defaultSpec testStetsonConfig2) { name = Just serverName }
  NewStyle -> GS.startLink $ (GS.defaultSpec testStetsonConfig) { name = Just serverName }
  NestedRoutes -> GS.startLink $ (GS.defaultSpec testStetsonConfigNested) { name = Just serverName }


stopLink :: Effect Unit
stopLink = GS.stop (ByName serverName)

routes ::
  { "TestBarebones" ::
      StetsonHandler Unit
        { handler :: String
        , userData :: Maybe String
        }
  , "TestFullyLoaded" ::
      StetsonHandler Unit
        { handler :: String
        , userData :: Maybe String
        }
  }
routes =
  { "TestBarebones": bareBonesHandler
  , "TestFullyLoaded": fullyLoadedHandler
  }

testStetsonConfig :: GS.InitFn Cont Stop Msg State
testStetsonConfig = do
  void
    $ liftEffect
    $ Stetson.startClear "http_listener"
    $ Stetson.configure
        { routes = Stetson.routes2 TestRoutes.apiRoute routes
        , bindPort = Port 3001
        }
  pure $ GS.InitOk $ State {}

--- Retain the old builder
testStetsonConfig2 :: GS.InitFn Cont Stop Msg State
testStetsonConfig2 = do
  void
    $ liftEffect
    $ Stetson.configure
    # Stetson.routes TestRoutes.apiRoute routes
    # Stetson.port (Port 3000)
    # Stetson.bindTo ip4Any
    # Stetson.cowboyRoutes cowboyRoutes
    # Stetson.startClear "http_listener2"
  pure $ GS.InitOk $ State {}

testStetsonConfigNested :: GS.InitFn Cont Stop Msg State
testStetsonConfigNested = do
  void
    $ liftEffect
    $ Stetson.startClear "http_listener3"
    $ Stetson.configure
        { routes =
          Stetson.routes2 TestRoutes.nestedApiRoute
            { "One": routes
            , "Two": routes
            }
        , bindPort = Port 3002
        }
  pure $ GS.InitOk $ State {}

--- Elementary rest handler
bareBonesHandler :: StetsonHandler Unit HandlerState
bareBonesHandler =
  routeHandler
    { init: initHandler
    , allowedMethods: \req state -> do Rest.result (Stetson.GET : nil) req state
    , contentTypesProvided: (\req state -> Rest.result (jsonWriter : nil) req state)
    }
  where
  initHandler req = Rest.initResult req { handler: "barebonesHandler", userData: Nothing }

-- Defining everything except loop & ws handlers
fullyLoadedHandler :: StetsonHandler Unit HandlerState
fullyLoadedHandler =
  routeHandler
    { init: initHandler
    , allowedMethods: \req state -> do Rest.result ((Stetson.POST : Stetson.HEAD : Stetson.GET : Stetson.OPTIONS : nil)) req state
    , allowMissingPost: restHandler false
    , deleteResource: \req state -> do Rest.result false req state
    , forbidden: restHandler false
    , isAuthorized: isAuthorizedHandler
    , isConflict: \req state -> restHandler (isConflict req state) req state
    , malformedRequest: restHandler false
    , movedPermanently: \req state -> Rest.result notMoved req state
    , movedTemporarily: \req state -> Rest.result notMoved req state
    , previouslyExisted: restHandler false
    , resourceExists: restHandler true
    , serviceAvailable: restHandler true
    , contentTypesAccepted: \req state -> do Rest.result ((tuple2 "application/json" acceptJson) : nil) req state
    , contentTypesProvided: (\req state -> Rest.result (jsonWriter : nil) req state)
    }
  where
  initHandler req = Rest.initResult req { handler: "fullyLoadedHandler", userData: Nothing }

  isAuthorizedHandler req state = Rest.result Authorized req state

  isConflict _req _state = false

  acceptJson req state = do
    body <- allBody req mempty
    _result <- either (pure <<< Left <<< show) handlePayload $ readJSON $ unsafeCoerce body
    Rest.result acceptSuccess req state

  handlePayload :: forall a. String -> Effect (Either a HandlerState)
  handlePayload payload = do
    pure $ Right { handler: "fullyLoadedHandler", userData: Just payload }

test2 :: SimpleStetsonHandler HandlerState
test2 =
  Rest.handler
    ( \req -> do
        -- And  our state can just be all the books in the library
        let
          state = { handler: "simpleHandler", userData: Nothing }
        -- Return our unmodified req along with our state
        Rest.initResult req state
    )
    -- Standard read/write methods
    
    # Rest.allowedMethods (\req state -> Rest.result (Stetson.HEAD : Stetson.GET : Stetson.OPTIONS : nil) req state)
    # Rest.contentTypesProvided (\req state -> Rest.result (jsonWriter : nil) req state)
    # Rest.contentTypesAccepted
        ( \req state ->
            Rest.result ((tuple2 "application/json" acceptJson) : nil)
              req
              state
        )
  where
  acceptJson req state = do
    -- Read the whole body, no buffering (how big can a book be??)
    body <- allBody req mempty
    -- read it as JSON, and chuck it into the create function of bookLibrary and obviously this is all
    -- either (Left/Right) all the way down
    result <- either (pure <<< Left <<< show) handlePayload $ readJSON $ unsafeCoerce body
    case result of
      -- The point being that Left -> Failure -> False -> Err as the body
      Left err -> Rest.result acceptFailure (setBody err req) state
      -- And Right -> Success -> True and no body
      Right _c -> Rest.result acceptSuccess req state

  handlePayload :: forall a. String -> Effect (Either a HandlerState)
  handlePayload payload = do
    pure $ Right { handler: "fullyLoadedHandler", userData: Just payload }

allBody :: Req -> IOData -> Effect Binary
allBody req acc = do
  readResult <- (readBody req)
  case readResult of
    (FullData body _req2) -> pure $ toBinary $ acc <> (fromBinary body)
    (PartialData body req2) -> (allBody req2 $ acc <> (fromBinary body))

-- Simple rest response
restHandler :: forall responseType state. responseType -> Req -> state -> Effect (RestResult responseType state)
restHandler val req state = Rest.result val req state

-- Simple init handler
cowboyRoutes :: List Path
cowboyRoutes = Path (tuple3 (matchSpec "/foo") (NativeModuleName $ atom "foo") (InitialState $ unsafeToForeign {})) : nil

jsonWriter :: forall a. WriteForeign a => Tuple2 String (Req -> a -> (Effect (RestResult IOData a)))
jsonWriter = tuple2 "application/json" (\req state -> Rest.result (fromString $ writeJSON state) req state)
