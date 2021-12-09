#!/usr/bin/env escript

%% https://adventofcode.com/2021/day/6

-mode(compile).

main(Args) ->
  Input = read_input(),
  %%io:format("Input:~n~p~n", [Input]),
  Ans =
    case Args of
      ["2"|_] -> solve_second(Input);
      _ -> solve_first(Input)
    end,
  io:format("~w~n", [Ans]).

read_input() ->
  {ok, [S]} = io:fread("", "~s"),
  [list_to_integer(N) || N <- string:split(S, ",", all)].

solve_first(Input) ->
  Queue = make_queue(Input),
  Evolve = evolve(Queue, 80),
  count(Evolve).

make_queue(Input) ->
  Base = maps:from_list([{N, 0} || N <- lists:seq(0, 8)]),
  Fold = fun(N, Map) -> maps:update_with(N, fun(X) -> X + 1 end, Map) end,
  Final = lists:foldl(Fold, Base, Input),
  {queue:from_list([maps:get(N, Final) || N <- lists:seq(0, 6)]),
   maps:get(7, Final), maps:get(8, Final)}.

evolve(Queue, 0) ->
  Queue;
evolve({Adult, Seven, Eight}, D) ->
  {{value, N}, NewQueue} = queue:out(Adult),
  FinalQueue = queue:in(N + Seven, NewQueue),
  evolve({FinalQueue, Eight, N}, D - 1).

count({Adult, Seven, Eight}) ->
  Seven + Eight + lists:sum(queue:to_list(Adult)).

solve_second(Input) ->
  Queue = make_queue(Input),
  Evolve = evolve(Queue, 256),
  count(Evolve).
