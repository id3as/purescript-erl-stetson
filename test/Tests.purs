module Test.Handlers where

import Prelude
import Data.Either (Either(..), either)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Erl.Atom (atom)
import Erl.Cowboy.Handlers.Rest (notMoved)
import Erl.Cowboy.Req (ReadBodyResult(..), Req, readBody, setBody)
import Erl.Cowboy.Routes (InitialState(..), Path(..), matchSpec)
import Erl.Data.Binary (Binary)
import Erl.Data.Binary.IOData (IOData, fromBinary, toBinary)
import Erl.Data.List (List, (:), nil)
import Erl.Data.Tuple (Tuple2, tuple2, tuple3)
import Erl.ModuleName (NativeModuleName(..))
import Foreign (Foreign, unsafeToForeign)
import Simple.JSON (class WriteForeign, readJSON, writeJSON)
import Stetson (Authorized(..), RestResult, SimpleStetsonHandler, StetsonHandler)
import Stetson as Stetson
import Stetson.Rest as Rest
import Stetson.Types (routeHandler)
import Test.TestRoutes as TestRoutes
import Unsafe.Coerce (unsafeCoerce)

type State
  = {}

type HandlerState
  = { handlerName :: String
    }

--- Elementary rest handler
bareBonesHandler :: StetsonHandler Unit HandlerState
bareBonesHandler =
  routeHandler
    { init: initHandler
    , allowedMethods: \req state -> do Rest.result (Stetson.GET : nil) req state
    , contentTypesProvided: (\req state -> Rest.result (jsonWriter : nil) req state)
    }
  where
  initHandler req = Rest.initResult req { handlerName: "bareBonesHandler" }

-- Defining everything except loop & ws handlers
fullyLoadedHandler :: StetsonHandler Unit HandlerState
fullyLoadedHandler =
  routeHandler
    { init: initHandler
    , allowedMethods: \req state -> do Rest.result (Stetson.POST : Stetson.HEAD : Stetson.GET : Stetson.OPTIONS : nil) req state
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
  initHandler req = Rest.initResult req { handlerName: "fullyLoadedHandler" }

  isAuthorizedHandler :: Req -> HandlerState -> Effect (RestResult Authorized HandlerState)
  isAuthorizedHandler req state = Rest.result Authorized req state

  isConflict :: Req -> HandlerState -> Boolean
  isConflict req state = false

  acceptJson :: forall state. Req -> state -> Effect (RestResult Boolean state)
  acceptJson req state = do
    body <- allBody req mempty
    result <- either (pure <<< Left <<< show) handlePayload $ readJSON $ unsafeCoerce body
    Rest.result true req state

  handlePayload :: String -> Effect (Either String HandlerState)
  handlePayload payload = do
    void $ logShow payload
    pure $ Right { handlerName: payload }

testStetsonConfig :: Effect (Either Foreign Unit)
testStetsonConfig = do
  Stetson.startClear "http_listener"
    $ Stetson.configure
        { routes =
          Stetson.routes2 TestRoutes.apiRoute
            { "TestBarebones": bareBonesHandler
            , "TestFullyLoaded": fullyLoadedHandler
            }
        }

--- Retain the old builder
testStetsonConfig2 :: Effect (Either Foreign Unit)
testStetsonConfig2 = do
  Stetson.configure
    # Stetson.routes TestRoutes.apiRoute
        ( { "TestBarebones": test2
          , "TestFullyLoaded": test2
          }
        )
    # Stetson.port 3000
    # Stetson.bindTo 0 0 0 0
    # Stetson.cowboyRoutes cowboyRoutes
    # Stetson.startClear "http_listener2"

test2 :: SimpleStetsonHandler HandlerState
test2 =
  Rest.handler
    ( \req -> do
        -- And  our state can just be all the books in the library
        let
          state = { handlerName: "test2" }
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
    result <- either (pure <<< Left <<< show) create $ readJSON $ unsafeCoerce body
    case result of
      -- The point being that Left -> Failure -> False -> Err as the body
      Left err -> Rest.result false (setBody err req) state
      -- And Right -> Success -> True and no body
      Right c -> Rest.result true req state

  create :: String -> Effect (Either String HandlerState)
  create payload = pure $ Right { handlerName: payload }

allBody :: Req -> IOData -> Effect Binary
allBody req acc = do
  readResult <- (readBody req)
  case readResult of
    (FullData body req2) -> pure $ toBinary $ acc <> (fromBinary body)
    (PartialData body req2) -> (allBody req2 $ acc <> (fromBinary body))

-- Simple rest response
restHandler :: forall responseType state. responseType -> Req -> state -> Effect (RestResult responseType state)
restHandler val req state = Rest.result val req state

-- Simple init hand
cowboyRoutes :: List Path
cowboyRoutes = Path (tuple3 (matchSpec "/foo") (NativeModuleName $ atom "foo") (InitialState $ unsafeToForeign {})) : nil

jsonWriter :: WriteForeign HandlerState => Tuple2 String (Req -> HandlerState -> (Effect (RestResult String HandlerState)))
jsonWriter = tuple2 "application/json" (\req state -> Rest.result (writeJSON state) req state)
