#!/usr/bin/env escript

%% https://adventofcode.com/2017/day/16

-mode(compile).

-define(SIZE, 16).

main(Args) ->
  Input = read_input(),
  io:format("~p~n", [length(Input)]),
  %%io:format("Input:~n~p~n", [Input]),
  Ans =
    case Args of
      ["2"|_] -> solve_second(Input);
      _ -> solve_first(Input)
    end,
  io:format("~p~n", [Ans]).

read_input() ->
  {ok, [S]} = io:fread("", "~s"),
  [parse(M) || M <- string:split(S, ",", all)].

parse("s" ++ Rest) ->
  {s, list_to_integer(Rest)};
parse("x" ++ Rest) ->
  [A, B] = [list_to_integer(N) || N <- string:split(Rest, "/")],
  {x, A, B};
parse("p" ++ Rest) ->
  [A, B] = string:split(Rest, "/"),
  {p, A, B}.

solve_first(Input) ->
  Letters = [[C] || C <- lists:seq($a, $a + ?SIZE - 1)],
  Pos = lists:seq(0, ?SIZE - 1),
  PToL = maps:from_list(lists:zip(Pos, Letters)),
  LToP = maps:from_list(lists:zip(Letters, Pos)),
  solve(Input, 0, PToL, LToP).

solve([], Offset, PToL, _) ->
  print(Offset, PToL);
solve([M|R], Offset, PToL, LToP) ->
  case M of
    {s, N} ->
      solve(R, (Offset + ?SIZE - N) rem ?SIZE, PToL, LToP);
    {x, A, B} ->
      PA = pos(Offset, A),
      PB = pos(Offset, B),
      CA = maps:get(PA, PToL),
      CB = maps:get(PB, PToL),
      NewPToL = PToL#{PA => CB, PB => CA},
      NewLToP = LToP#{CA => PB, CB => PA},
      solve(R, Offset, NewPToL, NewLToP);
    {p, A, B} ->
      PA = maps:get(A, LToP),
      PB = maps:get(B, LToP),
      NewPToL = PToL#{PA => B, PB => A},
      NewLToP = LToP#{A => PB, B => PA},
      solve(R, Offset, NewPToL, NewLToP)
  end.

print(Offset, PToL) ->
  Seq = lists:seq(0, ?SIZE - 1),
  Fold =
    fun(N, Acc) ->
        [C] = maps:get((N + Offset) rem ?SIZE, PToL),
        [C|Acc]
    end,
  lists:foldr(Fold, [], Seq).

pos(Offset, N) ->
  (Offset + N) rem ?SIZE.

solve_second(_Input) ->
  42.
