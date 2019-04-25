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

%-record(status, {bot, world, bot_status}).

%% API
-export([start/2, bot/1, world/1, execute/1]).

start(Bot, World) ->
	% Position {Line, Row}
	% ProgrammCounter Line
	BotStatus = {{1, 1}, 1},
	{Bot, World, BotStatus}.


bot({Bot, _World, _BotState}) ->
	Bot.


world({_Bot, World, _BotState}) ->
	World.


execute({Bot, World, {Pos, BotPC}}) ->
	Operation = genbot_bot:op(BotPC, Bot),
	% io:fwrite(">>> [~p] ~p~n", [BotPC, Operation]),
	NewBotPC = genbot_bot:increment_pc(Bot, BotPC, 1),
	execute(Operation, {Bot, World, {Pos, NewBotPC}}).

% ---

execute(nop, S) ->
	S;

execute(set, {Bot, World, S = {Pos, _BotPC}}) ->
	NewWorld = genbot_world:set(World, Pos, marked),
	{Bot, NewWorld, S};

execute({skip, Count}, {Bot, World, {Pos, BotPC}}) ->
	NewBotPC = genbot_bot:increment_pc(Bot, BotPC, Count),
	{Bot, World, {Pos, NewBotPC}};

execute({goto, BotPC}, {Bot, World, {Pos, _}}) ->
	{Bot, World, {Pos, BotPC}};

execute({skip_next_if, Direction, Status}, S = {Bot, World, {BotPos, BotPC}}) ->
	WorldPos = pos(BotPos, Direction),
	WorldStatus = genbot_world:get(World, WorldPos),
	case Status == WorldStatus of
		true ->
			NewBotPC = genbot_bot:increment_pc(Bot, BotPC, 1),
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


pos({Line, Row}, left) ->
	{Line, Row - 1};
pos({Line, Row}, right) ->
	{Line, Row + 1};
pos({Line, Row}, up) ->
	{Line - 1, Row};
pos({Line, Row}, down) ->
	{Line + 1, Row}.
