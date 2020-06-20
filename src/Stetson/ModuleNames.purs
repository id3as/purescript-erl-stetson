module Stetson.ModuleNames where

import Erl.ModuleName (ModuleName(..))

stetsonModuleNames :: ModuleName
stetsonModuleNames = ModuleName "Stetson.ModuleNames"

stetsonRest :: ModuleName
stetsonRest = ModuleName "Stetson.Rest"

stetsonHandlerProxy :: ModuleName
stetsonHandlerProxy = ModuleName "Stetson.HandlerProxy"

stetsonRouting :: ModuleName
stetsonRouting = ModuleName "Stetson.Routing"

stetsonTypes :: ModuleName
stetsonTypes = ModuleName "Stetson.Types"

stetsonWebSocket :: ModuleName
stetsonWebSocket = ModuleName "Stetson.WebSocket"

stetsonWebSocketHandler :: ModuleName
stetsonWebSocketHandler = ModuleName "Stetson.WebSocketHandler"
