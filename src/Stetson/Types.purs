module Stetson.Types ( RestResult(..)
               , InitResult(..)
               , InitHandler
               , AcceptHandler
               , ProvideHandler
               , RestHandler
               , WebSocketHandler
               , WebSocketInitHandler
               , WebSocketInfoHandler
               , WebSocketHandleHandler
               , WebSocketMessageRouter
               , WebSocketCallResult(..)
               , HttpMethod(..)
               , Authorized(..)
               , StetsonHandler(..)
               , InnerStetsonHandler(..)
               , ReceivingStetsonHandler(..)
               , StaticAssetLocation(..)
               , StetsonRoute
               , HandlerArgs
               , ConfiguredRoute(..)
               , StetsonConfig
               ) where

import Prelude
import Data.Maybe (Maybe)
import Effect (Effect)
import Erl.Cowboy.Handlers.Rest (MovedResult)
import Erl.Cowboy.Handlers.WebSocket (Frame)
import Erl.Cowboy.Req (Req)
import Erl.Cowboy.Routes (Path)
import Erl.Data.List (List)
import Erl.Data.Tuple (Tuple2, Tuple4)
import Erl.ModuleName (NativeModuleName)

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
  , movedTemporarily :: Maybe (Req -> state -> Effect (RestResult MovedResult state))
  , movedPermanently :: Maybe (Req -> state -> Effect (RestResult MovedResult state))
  , serviceAvailable :: Maybe (Req -> state -> Effect (RestResult Boolean state))
  , previouslyExisted :: Maybe (Req -> state -> Effect (RestResult Boolean state))
  , forbidden :: Maybe (Req -> state -> Effect (RestResult Boolean state))
  , isConflict :: Maybe (Req -> state -> Effect (RestResult Boolean state))
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


-- | Return type of most WebSocket callbacks
data WebSocketCallResult state = NoReply state
                               | Hibernate state
                               | Reply (List Frame) state
                               | ReplyAndHibernate (List Frame) state
                               | Stop state
  

-- | Router used to generate messages of the right type that'll appear
-- | in the info callback of a WebSocket handler
type WebSocketMessageRouter msg = (msg -> Effect Unit)

-- | Callback used to kick off the WebSocket handler, it is here where subscriptions should be 
-- | created, and in their callbacks the messages should be passed into the router for dealing with in the info callback
type WebSocketInitHandler msg state = WebSocketMessageRouter msg -> state -> Effect (WebSocketCallResult state)

-- | Callback used to handle messages sent from the client in the form of 'Frames' which will need 
-- | unpacking/decoding/parsing etc
type WebSocketHandleHandler msg state = Frame -> state -> Effect (WebSocketCallResult state)

-- | Callback used to handle messages sent from Erlang (hopefully via the router) so they'll be of the right type
type WebSocketInfoHandler msg state = msg -> state -> Effect (WebSocketCallResult state)


-- | Builder representing the complete list of callbacks for a WebSocket handler (okay not complete, feel free to add)
type WebSocketHandler msg state = {
    init :: InitHandler state
  , wsInit :: Maybe (WebSocketInitHandler msg state)
  , handle :: Maybe (WebSocketHandleHandler msg state)
  , info :: Maybe (WebSocketInfoHandler msg state)
  }


-- | For backwards compatability purposes, the default stetson handler 
-- | doesn't have a message type so it defaults to 'Unit'
type StetsonHandler state = InnerStetsonHandler Unit state

-- | A Stetson handler that can receive messages of type 'msg'
type ReceivingStetsonHandler msg state = InnerStetsonHandler msg state

-- | The actual type used to collect together handlers internally
data InnerStetsonHandler msg state = Rest (RestHandler state)
                                   | WebSocket (WebSocketHandler msg state)

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
