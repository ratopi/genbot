%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2019, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 24. Apr 2019 23:24
%%%-------------------------------------------------------------------
-module(genbot_simulator).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

-record(counts, {free_count, step_count}).

-record(result, {final_world, counts, fitness, bot}).

-record(set, {world, bots, bot_size}).

%% API
-export([create_start_set/2, execute/2, execute/1, evolve_bots/2, pick_winners/1, print/1, sort/1]).
-export([execute_bot/3]).


create_start_set(BotSize, BotCount) ->
	World = genbot_world:create_world(),
	Bots = create_some_bots(BotSize, BotCount),
	#set{world = World, bots = Bots, bot_size = BotSize}.


execute(Set, 0) ->
	io:fwrite("running loop ~p : ", [0]),
	Results = execute(Set),
	print_stastic(Results);

execute(Set, N) ->
	io:fwrite("running loop ~p : ", [N]),
	Results = execute(Set),
	print_stastic(Results),
	NewSet = evolve_bots(Set, Results),
	execute(NewSet, N - 1).


execute(Set) ->
	Result = run_bots(Set#set.world, Set#set.bots),
	sort(Result).


evolve_bots(Set, Results) ->
	BotSize = Set#set.bot_size,
	WinnerBots = [R#result.bot || R <- pick_winners(Results)],
	% NewBots = create_some_bots(BotSize, length(Results) - length(WinnerBots)),
	NewBots = mutate_bots(WinnerBots),
	#set{
		world = Set#set.world,
		bots = WinnerBots ++ NewBots,
		bot_size = BotSize
	}.

pick_winners(Results) ->
	SortedResults = sort(Results),
	lists:sublist(SortedResults, length(SortedResults) div 2).


print(Results) ->
	[io:fwrite("~p~n", [R#result.fitness]) || R <- Results],
	ok.

sort(Results) ->
	lists:sort(
		fun(R1, R2) ->
			R1#result.fitness =< R2#result.fitness
		end,
		Results
	).

% ---

create_some_bots(BotSize, BotCount) ->
	[genbot_bot:create_genbot(BotSize) || _ <- lists:seq(1, BotCount)].


run_bots(World, Bots) ->
	[
		self() ! next_please
		|| _ <- lists:seq(1, erlang:system_info(schedulers_online))
	],
	run_bots(World, Bots, [], length(Bots)).


run_bots(_World, [], Results, 0) ->
	Results;

run_bots(World, Bots, Results, Expected) ->
	receive
		next_please ->
			T = execute_bot_in_process(World, Bots),
			run_bots(World, T, Results, Expected);
		{result, Result} ->
			T = execute_bot_in_process(World, Bots),
			run_bots(World, T, [Result | Results], Expected - 1);
		X ->
			io:fwrite("received: ~p~n", [X]),
			run_bots(World, Bots, Results, Expected)
	end.


execute_bot_in_process(_World, []) ->
	[];

execute_bot_in_process(World, [Bot | T]) ->
	spawn(?MODULE, execute_bot, [World, Bot, self()]),
	T.


execute_bot(World, Bot, Pid) ->
	Result = execute_bot(World, Bot),
	Pid ! {result, Result}.



execute_bot(World, Bot) ->
	State = genbot_executor:start(Bot, World),
	Result = execute_bot_loop(State, 0),
	calculate_fitness(Result).



execute_bot_loop(State, 1000) ->
	World = genbot_executor:world(State),
	#result{
		%final_world = World,
		bot = genbot_executor:bot(State),
		counts = #counts{
			step_count = 1000,
			free_count = genbot_world:count_free_fields(World)
		}
	};

execute_bot_loop(State, N) ->
	% io:fwrite("~p   ", [N]),
	NewState = genbot_executor:execute(State),
	World = genbot_executor:world(NewState),
	case genbot_world:count_free_fields(World) of
		0 ->
			#result{
				%final_world = World,
				bot = genbot_executor:bot(NewState),
				counts = #counts{
					step_count = N,
					free_count = genbot_world:count_free_fields(World)
				}
			};
		_ ->
			execute_bot_loop(NewState, N + 1)
	end.


calculate_fitness(Result) ->
	Steps = Result#result.counts#counts.step_count,
	Fitness = (Steps / 1000.0) + Result#result.counts#counts.free_count,
	Result#result{fitness = Fitness}.


print_stastic(Results) ->
	N = length(Results),
	[BestResult | _] = Results,
	Fitnesses = [R#result.fitness || R <- Results],
	Average = lists:foldl(
		fun(F, A) ->
			A + F
		end,
		0,
		Fitnesses
	) / N,
	Min = lists:min(Fitnesses),
	Max = lists:max(Fitnesses),
	io:fwrite("<~p> (~p) Best: ~p [~p|~p]  Min: ~p  Avg: ~p  Max: ~p~n", [
		length(processes()),
		length(Results),
		BestResult#result.fitness,
		BestResult#result.counts#counts.free_count,
		BestResult#result.counts#counts.step_count,
		Min,
		Average,
		Max
	]).


mutate_bots(Bots) ->
	mutate_bots(Bots, []).


mutate_bots([], Acc) ->
	Acc;

mutate_bots([Bot | T], Acc) ->
	mutate_bots(
		T,
		[genbot_bot:mutate_bot(Bot) | Acc]
	).
