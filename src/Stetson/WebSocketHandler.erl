-module(stetson_webSocketHandler@foreign).

-export([self/0]).

self() -> fun() -> erlang:self() end.
