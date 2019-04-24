%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2019, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 24. Apr 2019 21:20
%%%-------------------------------------------------------------------
-module(genbot_world).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

%% API
-export([get/2, set/3, width/1, height/1, create_world/0, draw_world/1, count_free_fields/1, count_fields/2]).


width([H | _]) ->
	length(H).


height(World) ->
	length(World).


get(_World, {0, _Row}) ->
	blocked;

get(_World, {_Line, 0}) ->
	blocked;

get(World, {Line, Row}) ->
	case Line > height(World) of
		true ->
			blocked;
		false ->
			case Row > width(World) of
				true ->
					blocked;
				false ->
					WorldLine = lists:nth(Line, World),
					lists:nth(Row, WorldLine)
			end
	end.


set(World, {Line, Row}, Status) ->
	WorldLine = lists:nth(Line, World),
	NewWorldLine = exchange(WorldLine, Row, Status),
	exchange(World, Line, NewWorldLine).



count_free_fields(World) ->
	count_fields(World, free).


count_fields(World, Status) ->
	count_fields(World, Status, 0).



create_world() ->
	[
		[free, free, free, free, free, free, free, free, free],
		[free, free, free, free, free, free, free, free, free],
		[free, free, free, free, blocked, free, free, free, free],
		[free, free, free, free, blocked, free, free, free, free],
		[free, free, blocked, blocked, blocked, blocked, blocked, free, free],
		[free, free, free, free, blocked, free, free, free, free],
		[free, free, free, free, blocked, free, free, free, free],
		[free, free, free, free, free, free, free, free, free],
		[free, free, free, free, free, free, free, free, free]
	].

draw_world([]) ->
	ok;
draw_world([Line | T]) ->
	draw_world_line(Line),
	draw_world(T).


draw_world_line([]) ->
	io:fwrite("~n");
draw_world_line([H | T]) ->
	io:fwrite(show_field(H)),
	draw_world_line(T).


show_field(free) ->
	".";
show_field(marked) ->
	"*";
show_field(blocked) ->
	"X";
show_field(bot) ->
	"O".


exchange(List, Pos, Value) ->
	exchange(List, Pos, Value, 1, []).


exchange([], _Pos, _Value, _N, Acc) ->
	Acc;

exchange([_ | T], Pos, Value, Pos, Acc) ->
	exchange(T, Pos, Value, Pos + 1, Acc ++ [Value]);

exchange([H | T], Pos, Value, N, Acc) ->
	exchange(T, Pos, Value, N + 1, Acc ++ [H]).



count_fields([], _Status, N) ->
	N;

count_fields([L = [_ | _] | T], Status, N) ->
	NewN = count_fields(L, Status, N),
	count_fields(T, Status, NewN);

count_fields([Status | T], Status, N) ->
	count_fields(T, Status, N + 1);

count_fields([_ | T], Status, N) ->
	count_fields(T, Status, N).
