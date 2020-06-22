-module(stetson_handlerProxy@foreign).

-export([self/0
       , restInitResult/2
       , loopInitResult/2
       , wsInitResult/2
        ]).

self() -> fun() -> erlang:self() end.

restInitResult(State, Req) ->
  {cowboy_rest, Req, State}.

wsInitResult(State, Req) ->
  {cowboy_websocket, Req, State}.

loopInitResult(State, Req) ->
  {cowboy_loop, Req, State}.
