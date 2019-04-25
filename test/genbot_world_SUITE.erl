%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2019, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 26. Apr 2019 00:01
%%%-------------------------------------------------------------------
-module(genbot_world_SUITE).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

-include_lib("common_test/include/ct.hrl").

-record(bot, {ops}).

%% API
-export([init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2, all/0]).
-export([count_fields/1]).

all() -> [
	count_fields
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

count_fields(_Config) ->
	W = genbot_world:create_world(),
	72 = genbot_world:count_free_fields(W),
	W2 = genbot_world:set(W, {1, 1}, marked),
	72 = genbot_world:count_free_fields(W),
	W3 = genbot_world:set(W2, {2, 1}, marked),
	70 = genbot_world:count_free_fields(W3).
