#!/usr/bin/env escript

%% https://adventofcode.com/2021/day/13

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
    fun([Line], {Map, Folds}) ->
        case Line of
          "fold" -> {Map, Folds};
          "along" -> {Map, Folds};
          [D,$=|VS] ->
            V = list_to_integer(VS),
            {Map, [{D,V}|Folds]};
          _ ->
            [X, Y] = [list_to_integer(N) || N <- string:split(Line, ",")],
            {Map#{{X, Y} => true}, Folds}
        end
    end,
  {Map, Folds} = lists:foldl(Fold, {#{}, []}, Lines),
  {Map, lists:reverse(Folds)}.

solve_first({Map, [Fold|_]}) ->
  New = fold(Fold, Map),
  count(New).

fold({D, V}, Map) ->
  Fold =
    fun({X, Y}, _, Acc) ->
        case D of
          $x ->
            case X > V of
              true ->
                Diff = abs(V - X),
                NX = V - Diff,
                Acc#{{NX, Y} => true};
              false ->
                Acc#{{X, Y} => true}
            end;
          $y ->
            case Y > V of
              true ->
                Diff = abs(V - Y),
                NY = V - Diff,
                Acc#{{X, NY} => true};
              false ->
                Acc#{{X, Y} => true}
            end
        end
    end,
  maps:fold(Fold, #{}, Map).

count(Map) ->
  maps:size(Map).

solve_second({Map, Folds}) ->
  NMap = lists:foldl(fun fold/2, Map, Folds),
  print(NMap).

print(Map) ->
  MaxFold = fun({X, Y}, _, {XM, YM}) -> {max(X, XM), max(Y, YM)} end,
  {XM, YM} = maps:fold(MaxFold, {0, 0}, Map),
  Foreach =
    fun(Y) ->
        Foreach2 =
          fun(X) ->
              case maps:get({X, Y}, Map, false) of
                true -> io:format("#");
                false -> io:format(" ")
              end
          end,
          lists:foreach(Foreach2, lists:seq(0, XM)),
        io:format("~n")
    end,
  lists:foreach(Foreach, lists:seq(0, YM)),
  0.
