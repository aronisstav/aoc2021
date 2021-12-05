#!/usr/bin/env escript

%% https://adventofcode.com/2021/day/1

-mode(compile).

main(Args) ->
  Input = read_list("~d"),
  Ans =
    case Args of
      ["2"] -> solve_second(Input);
      _ -> solve_first(Input)
    end,
  io:format("~w~n", [Ans]).

read_list(Pat) -> read_list(Pat, []).

read_list(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Res} -> read_list(Pat, [Res|Acc]);
    eof -> lists:append(lists:reverse(Acc))
  end.

solve_first(Input) ->
  count_increases(Input, 0).

count_increases([_], Count) -> Count;
count_increases([X,Y|T], Count) ->
  NewCount =
    case Y > X of
      true -> Count + 1;
      false -> Count
    end,
  NewNums = [Y|T],
  count_increases(NewNums, NewCount).

solve_second(Input) ->
  Sliding = sliding_3(Input),
  count_increases(Sliding, 0).

sliding_3([X,Y,Z|T]) ->
  Sum = X + Y + Z,
  sliding_3(Sum, X, Y, Z, T, []).

sliding_3(Sum, _, _, _, [], Acc) ->
  lists:reverse([Sum|Acc]);
sliding_3(Sum, X, Y, Z, [W|T], Acc) ->
  NewSum = Sum - X + W,
  sliding_3(NewSum, Y, Z, W, T, [Sum|Acc]).
