#!/usr/bin/env escript

%% https://adventofcode.com/2021/day/2

-mode(compile).

main(Args) ->
  Input = read_input(),
  Ans =
    case Args of
      ["2"] -> solve_second(Input);
      _ -> solve_first(Input)
    end,
  io:format("~w~n", [Ans]).

read_input() ->
  [{D, X} || [D, X] <- read_list("~a ~d")].

read_list(Pat) -> read_list(Pat, []).

read_list(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Res} -> read_list(Pat, [Res|Acc]);
    eof -> lists:reverse(Acc)
  end.

solve_first(Input) ->
  Fold =
    fun({M, X}, {H, D}) ->
        case M of
          forward -> {H + X, D};
          down -> {H, D + X};
          up -> {H, D - X}
        end
    end,
  {FH, FD} = lists:foldl(Fold, {0, 0}, Input),
  FH * FD.

solve_second(Input) ->
  Fold =
    fun({M, X}, {H, D, A}) ->
        case M of
          forward -> {H + X, D + X * A, A};
          down -> {H, D, A + X};
          up -> {H, D, A - X}
        end
    end,
  {FH, FD, _} = lists:foldl(Fold, {0, 0, 0}, Input),
  FH * FD.
