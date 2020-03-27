-module(routingDuplexMiddleware@foreign).
-export([cowboyRouterExecute/2]).

cowboyRouterExecute(Req, Env) ->
  fun() ->
    cowboy_router:execute(Req,Env)
  end.