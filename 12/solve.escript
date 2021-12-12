#!/usr/bin/env escript

%% https://adventofcode.com/2021/day/12

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
  Pat = "~s",
  Lines = read_lines(Pat, []),
  to_map(Lines).

read_lines(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Inputs} -> read_lines(Pat, [Inputs|Acc]);
    eof -> lists:reverse(Acc)
  end.

to_map(Lines) ->
  Fold =
    fun([Line], Map) ->
        [A, B] = string:split(Line, "-"),
        add_edge(B, A, add_edge(A, B, Map))
    end,
  lists:foldl(Fold, #{}, Lines).

add_edge(A, B, Map) ->
  Upd = fun(V) -> [B|V] end,
  maps:update_with(A, Upd, [B], Map).

solve_first(Input) ->
  length(lists:usort(unique_paths("start", ["start"], false, Input))).

unique_paths("end", _, _, _) ->
  [["end"]];
unique_paths(From, Forbid, Choice, Map) ->
  Neighs = maps:get(From, Map, []) -- Forbid,
  NewForbid =
    case From of
      [L|_] when L >= $a, L =< $z -> [From|Forbid];
      _ -> Forbid
    end,
  Follows =
    case {From, Choice} of
      {NotStart, true} when NotStart =/= "start", NewForbid =/= Forbid ->
        [unique_paths(N, NewForbid, true, Map) || N <- Neighs] ++
          [unique_paths(N, Forbid, false, Map) || N <- Neighs];
      _ -> [unique_paths(N, NewForbid, Choice, Map) || N <- Neighs]
    end,
  %%io:format("~p~n", [{From, Follows}]),
  [[From|F] || F <- lists:append(Follows)].

solve_second(Input) ->
  length(lists:usort(unique_paths("start", ["start"], true, Input))).
