%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2019, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 24. Apr 2019 21:28
%%%-------------------------------------------------------------------
-module(genbot_executor).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

%% API
-export([start/2, execute/1]).

start(Bot, World) ->
	% Position {Line, Row}
	% ProgrammCounter Line
	BotStatus = {{1, 1}, 1},
	{Bot, World, BotStatus}.


execute({Bot, World, {Pos, BotPC}}) ->
	Operation = lists:nth(BotPC, Bot),
	NewBotPC = increment_pc(Bot, BotPC),
	execute(Operation, {Bot, World, {Pos, NewBotPC}}).


execute(nop, S) ->
	S;

execute(set, {Bot, World, S = {Pos, _BotPC}}) ->
	NewWorld = genbot_world:set(World, Pos, marked),
	{Bot, NewWorld, S};

execute({skip, Count}, {Bot, World, {Pos, BotPC}}) ->
	NewBotPC = increment_pc(Bot, BotPC, Count),
	{Bot, World, {Pos, NewBotPC}};

execute({skip_next_if, Direction, Status}, S = {Bot, World, {BotPos, BotPC}}) ->
	WorldPos = pos(BotPos, Direction),
	WorldStatus = genbot_world:get(World, WorldPos),
	case Status == WorldStatus of
		true ->
			NewBotPC = increment_pc(Bot, BotPC),
			{Bot, World, {BotPos, NewBotPC}};
		false ->
			S
	end;

execute({move, Direction}, S = {Bot, World, {Pos, BotPC}}) ->
	NewPos = pos(Pos, Direction),
	case genbot_world:get(World, NewPos) of
		free -> {Bot, World, {NewPos, BotPC}};
		marked -> {Bot, World, {NewPos, BotPC}};
		blocked -> S
	end.


increment_pc(Bot, BotPC) ->
	increment_pc(Bot, BotPC, 1).


increment_pc(Bot, BotPC, N) ->
	(BotPC + N) rem length(Bot).


pos({Line, Row}, left) ->
	{Line, Row - 1};
pos({Line, Row}, right) ->
	{Line, Row + 1};
pos({Line, Row}, up) ->
	{Line - 1, Row};
pos({Line, Row}, down) ->
	{Line + 1, Row}.
