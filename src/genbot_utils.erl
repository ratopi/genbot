%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2019, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 25. Apr 2019 22:39
%%%-------------------------------------------------------------------
-module(genbot_utils).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

%% API
-export([exchange/3]).


exchange(List, Pos, Value) ->
	exchange(List, Pos, Value, 1, []).


exchange([], _Pos, _Value, _N, Acc) ->
	Acc;

exchange([_ | T], Pos, Value, Pos, Acc) ->
	exchange(T, Pos, Value, Pos + 1, Acc ++ [Value]);

exchange([H | T], Pos, Value, N, Acc) ->
	exchange(T, Pos, Value, N + 1, Acc ++ [H]).
