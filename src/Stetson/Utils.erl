-module(stetson_utils@foreign).

-export([unsafeMergeOptional/2]).

unsafeMergeOptional(Defaults,Optionals) ->
  maps:fold(fun (K, V, R) ->
              case maps:is_key(K, Optionals) of
                true -> maps:put(K, {just, maps:get(K, Optionals)}, R);
                false -> maps:put(K, V, R)
              end
            end,
            Optionals,
            Defaults).

