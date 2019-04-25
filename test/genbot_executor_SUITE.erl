%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2019, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 25. Apr 2019 00:11
%%%-------------------------------------------------------------------
-module(genbot_executor_SUITE).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

-include_lib("common_test/include/ct.hrl").

-record(bot, {ops, all_ops}).
-record(status, {bot, world, bot_status}).

%% API
-export([init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2, all/0]).
-export([test_move/1, test_move_border/1]).

all() -> [
	test_move,
	test_move_border
].

% ---

init_per_suite(Config) ->
	Config.

end_per_suite(_Config) ->
	ok.

init_per_testcase(_, Config) ->
	Config.

end_per_testcase(_, _Config) ->
	ok.

% ---

test_move(_Config) ->
	World = [
		[free, free, free],
		[free, free, free],
		[free, free, free]
	],
	StartPos = {2, 2},
	test_move(World, StartPos, right, {2, 3}),
	test_move(World, StartPos, left, {2, 1}),
	test_move(World, StartPos, up, {1, 2}),
	test_move(World, StartPos, down, {3, 2}).


test_move_border(_Config) ->
	World = [
		[free, free],
		[free, free]
	],
	test_move(World, {1, 2}, right, {1, 2}),
	test_move(World, {1, 1}, left, {1, 1}),
	test_move(World, {1, 1}, up, {1, 1}),
	test_move(World, {2, 1}, down, {2, 1}).


test_move(World, StartPos, Direction, TargetPos) ->
	Bot = #bot{ops = [{move, Direction}]},
	BotStatus = {StartPos, 1},
	Status = {Bot, World, BotStatus},
	NewBotStatus = {TargetPos, 1},
	{Bot, World, NewBotStatus} = genbot_executor:execute(Status).
