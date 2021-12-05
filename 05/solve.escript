#!/usr/bin/env escript

%% https://adventofcode.com/2021/day/5

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
  [{{A, B}, {C, D}} || [A, B, C, D] <- read_list("~d,~d -> ~d,~d")].

read_list(Pat) -> read_list(Pat, []).

read_list(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Res} -> read_list(Pat, [Res|Acc]);
    eof -> lists:reverse(Acc)
  end.

solve_first(Input) ->
  Board = make_board(Input),
  count(Board).

make_board(Input) ->
  Fold =
    fun(Line, Acc) ->
        case Line of
          {{X, Y1}, {X, Y2}} ->
            Min = min(Y1, Y2),
            Max = max(Y1, Y2),
            add([{X, Y} || Y <- lists:seq(Min, Max)], Acc);
          {{X1, Y}, {X2, Y}} ->
            Min = min(X1, X2),
            Max = max(X1, X2),
            add([{X, Y} || X <- lists:seq(Min, Max)], Acc);
          _ -> Acc
        end
    end,
  lists:foldl(Fold, #{}, Input).

add(Points, Map) ->
  Fold =
    fun(C, Acc) ->
        maps:update_with(C, fun(X) -> X + 1 end, 1, Acc)
    end,
  lists:foldl(Fold, Map, Points).

count(Map) ->
  Fold =
    fun(_, V, Acc) ->
        case V > 1 of
          true -> Acc + 1;
          false -> Acc
        end
    end,
  maps:fold(Fold, 0, Map).

solve_second(Input) ->
  Board = make_full_board(Input),
  count(Board).

make_full_board(Input) ->
  Fold =
    fun(Line, Acc) ->
        case Line of
          {{X, Y1}, {X, Y2}} ->
            Min = min(Y1, Y2),
            Max = max(Y1, Y2),
            add([{X, Y} || Y <- lists:seq(Min, Max)], Acc);
          {{X1, Y}, {X2, Y}} ->
            Min = min(X1, X2),
            Max = max(X1, X2),
            add([{X, Y} || X <- lists:seq(Min, Max)], Acc);
          {{X1, Y1}, {X2, Y2}} ->
            Xs =
              case X1 < X2 of
                true -> lists:seq(X1, X2);
                false -> lists:seq(X1, X2, -1)
              end,
            Ys =
              case Y1 < Y2 of
                true -> lists:seq(Y1, Y2);
                false -> lists:seq(Y1, Y2, -1)
              end,
            Points = lists:zip(Xs, Ys),
            add(Points, Acc)
        end
    end,
  lists:foldl(Fold, #{}, Input).
