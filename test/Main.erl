-module(test_main@foreign).
-export([startup/0]).

startup() -> fun() ->
  {ok, _} = application:ensure_all_started(cowboy)
end.
