module Stetson.WebSocket where
  
import Prelude
import Effect (Effect)
import Erl.Cowboy.Req (Req)
import Data.Maybe (Maybe(..))
import Stetson (InitHandler, InitResult(..), InnerStetsonHandler(..), WebSocketHandleHandler, WebSocketHandler, WebSocketInfoHandler, WebSocketInitHandler)


handler :: forall msg state. InitHandler state -> WebSocketHandler msg state
handler i = { init: i
               , wsInit: Nothing
               , handle: Nothing
               , info : Nothing
               }

init  :: forall msg state. WebSocketInitHandler msg state -> WebSocketHandler msg state -> WebSocketHandler msg state
init fn h = h { wsInit = Just fn }

handle :: forall msg state.  WebSocketHandleHandler msg state -> WebSocketHandler msg state -> WebSocketHandler msg state 
handle fn h = 
  h { handle = Just fn  }

info :: forall msg state.  WebSocketInfoHandler msg state -> WebSocketHandler msg state -> WebSocketHandler msg state 
info fn h = 
  h { info = Just fn  }

yeeha :: forall msg state. WebSocketHandler msg state -> InnerStetsonHandler msg state
yeeha = WebSocket

initResult :: forall state. Req -> state -> Effect (InitResult state)
initResult rq st = pure $ InitOk rq st
