#!/usr/bin/env escript

%% https://adventofcode.com/2021/day/22

-mode(compile).

main(Args) ->
  Input = read_input(),
  Ans =
    case Args of
      ["2"|_] -> solve_second(Input);
      _ -> solve_first(Input)
    end,
  io:format("~w~n", [Ans]).

read_input() ->
  L = read_lines("~a x=~d..~d,y=~d..~d,z=~d..~d", []),
  [{A, [{XL, XH}, {YL, YH}, {ZL, ZH}]} || [A, XL, XH, YL, YH, ZL, ZH] <- L].

read_lines(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Inputs} -> read_lines(Pat, [Inputs|Acc]);
    eof -> lists:reverse(Acc)
  end.

solve_first(Input) ->
  Clean = clean(Input),
  Final = boot50(Clean),
  count(Final).

clean(Input) ->
  [{A, [{XL, XH}, {YL, YH}, {ZL, ZH}]} ||
    {A, [{XL, XH}, {YL, YH}, {ZL, ZH}]} <- Input,
    XL > -51,
    YL > -51,
    ZL > -51,
    XH < 51,
    YH < 51,
    ZH < 51].

boot50(Input) ->
  Fold =
    fun({S, Limits}, Acc) ->
        [Xs, Ys, Zs] = [lists:seq(L, H) || {L, H} <- Limits],
        V = case S of on -> 1; off -> 0 end,
        FF = fun(Coords, AccIn) -> AccIn#{Coords => V} end,
        Cs = [{X, Y, Z} || X <- Xs, Y <- Ys, Z <- Zs],
        lists:foldl(FF, Acc, Cs)
    end,
  lists:foldl(Fold, #{}, Input).

count(Final) ->
  Fold = fun(_, V, Acc) -> V + Acc end,
  maps:fold(Fold, 0, Final).

solve_second(Input) ->
  Intervals = make_intervals(Input),
  Map = light(Intervals, Input),
  count_regions(Map).

make_intervals(Input) ->
  Base = [{-200000,200000}],
  Fold =
    fun({_, [X, Y, Z]}, {XAcc, YAcc, ZAcc}) ->
        {add_int(X, XAcc), add_int(Y, YAcc), add_int(Z, ZAcc)}
    end,
  lists:foldl(Fold, {Base, Base, Base}, Input).

add_int({L, H}, Int) ->
  WithL = cut_h(L, Int),
  cut_l(H, WithL).

cut_l(N, Int) ->
  cut_l(N, Int, []).

cut_h(N, Int) ->
  cut_h(N, Int, []).

cut_h(N, [{L, H}|R], Acc) ->
  if N > H ->
      cut_h(N, R, [{L, H}|Acc]);
     N =:= H ->
      lists:reverse(Acc, [{L, H}|R]);
     N < H ->
      lists:reverse(Acc, [{L, N},{N + 1, H}|R])
  end.

cut_l(N, [{L, H}|R], Acc) ->
  if N < L ->
      lists:reverse(Acc,[{N, L - 1}, {L, H}|R]);
     N =:= L -> lists:reverse(Acc, [{L, H}|R]);
     N < H ->
      lists:reverse(Acc, [{L, N - 1}, {N, H}|R]);
     true ->
      cut_l(N, R, [{L, H}|Acc])
  end.

light({Xs, Ys, Zs}, Input) ->
  FoldZ =
    fun(Z, AccZ) ->
        FoldY =
          fun(Y, AccY) ->
              FoldX =
                fun(X, AccX) -> AccX#{{X, Y, Z} => 0} end,
              lists:foldl(FoldX, AccY, Xs)
          end,
        lists:foldl(FoldY, AccZ, Ys)
    end,
  lists:foldl(FoldZ, #{}, Zs).

count_regions(Map) ->
  Map.
