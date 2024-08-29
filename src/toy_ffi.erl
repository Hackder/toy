-module(toy_ffi).
-export([index/2, is_nullish/1, decode_option/1, decode_map/1]).

index(Tuple, Index) when is_tuple(Tuple) andalso is_integer(Index) ->
    try
        {ok, {some, element(Index + 1, Tuple)}}
    catch _:_ -> 
        {ok, none}
    end;
index(Map, Key) when is_map(Map) ->
    try
        {ok, {some, maps:get(Key, Map)}}
    catch _:_ -> 
        {ok, none}
    end;
index(_, Index) when is_integer(Index) ->
    {error, <<"Indexable">>};
index(_, _) ->
    {error, <<"Dict">>}.

is_nullish(Value) ->
  case Value of
    undefined -> true;
    null -> true;
    nil -> true;
    _ -> false
  end.

decode_option(Value) ->
  case Value of
    {some, Inner} -> {ok, {some, Inner}};
    none -> {ok, none};
    _ -> {error, nil}
  end.

decode_map(Data) when is_map(Data) -> {ok, Data};
decode_map(Data) -> {error, [{toy_error, {invalid_type, <<"Dict">>, gleam_stdlib:classify_dynamic(Data)}, []}]}.
