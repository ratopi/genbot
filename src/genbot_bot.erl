%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2019, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 24. Apr 2019 20:53
%%%-------------------------------------------------------------------
-module(genbot_bot).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

-record(bot, {ops, all_ops}).

%% API
-export([create_genbot/1, increment_pc/3, op_counts/1, op/2, mutate_bot/1]).

create_genbot(Size) ->
	O = op_codes(Size div 3, Size),
	Ops = [select_one(O) || _ <- lists:seq(1, Size)],
	#bot{ops = Ops, all_ops = O}.


op_counts(Bot) ->
	length(Bot#bot.ops).


op(PC, Bot) ->
	lists:nth(PC, Bot#bot.ops).


increment_pc(Bot, BotPC, Increment) ->
	check_borders(Bot, BotPC + Increment).

mutate_bot(Bot) ->
	mutate_bot(Bot, random:uniform(length(Bot#bot.ops))).


% ---

mutate_bot(Bot, 0) ->
	Bot;

mutate_bot(Bot, N) ->
	Step = random:uniform(op_counts(Bot)),
	AllOps = Bot#bot.all_ops,
	NewOps =
		genbot_utils:exchange(
			Bot#bot.ops,
			Step,
			select_one(AllOps)
		),
	mutate_bot(
		#bot{ops = NewOps, all_ops = AllOps},
		N - 1
	).


select_one(OpCodes) ->
	N = random:uniform(length(OpCodes)),
	lists:nth(N, OpCodes).


check_borders(Bot, PC) when PC < 1 ->
	op_counts(Bot);


check_borders(Bot, PC) ->
	case PC > op_counts(Bot) of
		true -> 1;
		false -> PC
	end.


op_codes(MaxSkip, LineCount) ->
	[nop, set] ++
		[{move, D} || D <- directions()] ++
		% [{skip, C} || C <- lists:seq(1, MaxSkip)] ++
		[{skip, C} || C <- lists:seq(1, LineCount)] ++
		[{skip_next_if, D, S} || D <- directions(), S <- status()].


directions() ->
	[left, right, up, down].

status() ->
	[free, marked, blocked].
