#!/usr/bin/env escript

%% https://adventofcode.com/2021/day/7

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
  Median = median(Input),
  cost(Median, Input).

median(Input) ->
  Sort = lists:sort(Input),
  L = length(Input),
  N = round(L / 2),
  lists:nth(N, Sort).

cost(Average, Input) ->
  Fold = fun(X, Cost) -> Cost + abs(X - Average) end,
  lists:foldl(Fold, 0, Input).

solve_second(Input) ->
  Min = lists:min(Input),
  Max = lists:max(Input),
  Fold =
    fun(C, Best) ->
        Cost = cost2(C, Input),
        min(Best, Cost)
    end,
  Base = cost2(Min, Input),
  lists:foldl(Fold, Base, lists:seq(Min + 1, Max)).

cost2(C, Input) ->
  Fold =
    fun(N, Sum) ->
        D = abs(N - C),
        Sum + round(D *(D + 1) / 2)
    end,
  lists:foldl(Fold, 0, Input).
