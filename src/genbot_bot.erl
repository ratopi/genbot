%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2019, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 24. Apr 2019 20:{
%%%-------------------------------------------------------------------
-module(genbot_bot).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

%% API
-export([create_genbot/1, op_codes/1]).

create_genbot(Size) ->
	O = op_codes(Size),
	[select_one(O) || N <- lists:seq(1, Size)].


select_one(OpCodes) ->
	N = random:uniform(length(OpCodes)),
	lists:nth(N, OpCodes).



op_codes(MaxSkip) ->
	[nop, set] ++
		[{move, D} || D <- directions()] ++
		[{skip, C} || C <- lists:seq(1, MaxSkip)] ++
		[{skip_next_if, D, S} || D <- directions(), S <- status()].


directions() ->
	[left, right, up, down].

status() ->
	[free, marked, blocked].
