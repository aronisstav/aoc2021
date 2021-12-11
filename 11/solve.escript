#!/usr/bin/env escript

%% https://adventofcode.com/2021/day/11

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

to_map(Lines) ->
  Fold =
    fun([Line], {Y, _, Map}) ->
        IFold =
          fun(S, {X, IMap}) ->
              {X + 1, IMap#{{X, Y} => S - $0}}
          end,
        {L, OMap} = lists:foldl(IFold, {1, Map}, Line),
        {Y + 1, L, OMap}
    end,
  {YM, XM, OMap} = lists:foldl(Fold, {1, 0, #{}}, Lines),
  {XM - 1, YM - 1, OMap}.

read_lines(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Inputs} -> read_lines(Pat, [Inputs|Acc]);
    eof -> lists:reverse(Acc)
  end.

solve_first(Input) ->
  count_flashes(100, Input, 0).

count_flashes(0, _, Acc) ->
  Acc;
count_flashes(N, Map, Acc) ->
  NMap = increment(Map),
  FMap = flash(NMap),
  {C, RMap} = reset(FMap),
  count_flashes(N - 1, RMap, Acc + C).

increment({XM, XM, Map}) ->
  Scan = lists:seq(1, XM),
  Cs = [{X, Y} || X <- Scan, Y <- Scan],
  Inc = fun(X) -> X + 1 end,
  Fold = fun(P, MapAcc) -> maps:update_with(P, Inc, MapAcc) end,
  {XM, XM, lists:foldl(Fold, Map, Cs)}.

flash(NMap) ->
  Ready = find_ready(NMap),
  flash(Ready, NMap).

find_ready({XM, XM, Map}) ->
  Scan = lists:seq(1, XM),
  Cs = [{X, Y} || X <- Scan, Y <- Scan],
  Fold =
    fun(P, Acc) ->
        #{P := V} = Map,
        case V =:= 10 of
          true -> [P|Acc];
          false -> Acc
        end
    end,
  lists:foldl(Fold, [], Cs).

flash([], Map) -> Map;
flash([{X, Y}|R], {XM, YM, Map}) ->
  Ns = find_ns(X, Y, XM, YM),
  Fold =
    fun(P, {RAcc, MapAcc}) ->
        #{P := V} = MapAcc,
        NV = V + 1,
        NRAcc =
          case NV =:= 10 of
            true -> [P|RAcc];
            false -> RAcc
          end,
        NMapAcc = MapAcc#{P => NV},
        {NRAcc, NMapAcc}
    end,
  {NR, NMap} = lists:foldl(Fold, {[], Map}, Ns),
  flash(R ++ NR, {XM, YM, NMap}).

find_ns(X, Y, XM, YM) ->
  Diffs = lists:seq(-1, 1),
  Cs = [{X + XX, Y + YY} || XX <- Diffs, YY <- Diffs, {XX, YY} =/= {0, 0}],
  lists:append([maybe(XX, YY, XM, YM) || {XX, YY} <- Cs]).

maybe(0, _, _, _) -> [];
maybe(_, 0, _, _) -> [];
maybe(X, _, XM, _) when X > XM -> [];
maybe(_, Y, _, YM) when Y > YM -> [];
maybe(X, Y, _, _) -> [{X, Y}].

reset({XM, XM, Map}) ->
  Scan = lists:seq(1, XM),
  Cs = [{X, Y} || X <- Scan, Y <- Scan],
  Fold =
    fun(P, {C, Acc}) ->
        #{P := V} = Acc,
        case V > 9 of
          true -> {C + 1, Acc#{P => 0}};
          false -> {C, Acc}
        end
    end,
  {C, FMap} = lists:foldl(Fold, {0, Map}, Cs),
  {C, {XM, XM, FMap}}.

solve_second(Input) ->
  sync(1, Input).

sync(N, Map) ->
  NMap = increment(Map),
  FMap = flash(NMap),
  {C, RMap} = reset(FMap),
  case C =:= 100 of
    true -> N;
    false -> sync(N + 1, RMap)
  end.
