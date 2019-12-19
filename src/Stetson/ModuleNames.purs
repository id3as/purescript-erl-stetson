module Stetson.ModuleNames where

import Erl.ModuleName (ModuleName(..))

stetsonModuleNames :: ModuleName
stetsonModuleNames = ModuleName "Stetson.ModuleNames"

stetsonRest :: ModuleName
stetsonRest = ModuleName "Stetson.Rest"

stetsonRestHandler :: ModuleName
stetsonRestHandler = ModuleName "Stetson.RestHandler"

stetsonWebSocketHandler :: ModuleName
stetsonWebSocketHandler = ModuleName "Stetson.WebSocketHandler"
