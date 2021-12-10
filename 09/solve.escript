#!/usr/bin/env escript

%% https://adventofcode.com/2021/day/9

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
  local_minima_sum(Input).

local_minima_sum({XM, YM, Map}) ->
  Fold =
    fun(P, Acc) ->
        Acc + add_if_minima(P, XM, YM, Map)
    end,
  lists:foldl(Fold, 0, [{X, Y} || X <- lists:seq(1, XM),
                                  Y <- lists:seq(1, YM)]).

add_if_minima({X, Y}, XM, YM, Map) ->
  Ns = find_ns(X, Y, XM, YM),
  NVs = [maps:get(P, Map) || P <- Ns],
  V = maps:get({X, Y}, Map),
  case lists:any(fun(NV) -> NV =< V end, NVs) of
    true -> 0;
    false -> V + 1
  end.

find_ns(X, Y, XM, YM) ->
  T = maybe(X, Y - 1, XM, YM),
  B = maybe(X, Y + 1, XM, YM),
  R = maybe(X + 1, Y, XM, YM),
  L = maybe(X - 1, Y, XM, YM),
  T ++ B ++ R ++ L.

maybe(0, _, _, _) -> [];
maybe(_, 0, _, _) -> [];
maybe(X, _, XM, _) when X > XM -> [];
maybe(_, Y, _, YM) when Y > YM -> [];
maybe(X, Y, _, _) -> [{X, Y}].

solve_second(Input) ->
  Minima = find_minima(Input),
  Sizes = [basin(M, Input) || M <- Minima],
  [A, B, C|_] = lists:reverse(lists:sort(Sizes)),
  A * B * C.

find_minima({XM, YM, Map}) ->
  Fold =
    fun(P, Acc) ->
        case 0 =:= add_if_minima(P, XM, YM, Map) of
          true -> Acc;
          false -> [P|Acc]
        end
    end,
  lists:foldl(Fold, [], [{X, Y} || X <- lists:seq(1, XM),
                                  Y <- lists:seq(1, YM)]).

basin(P, Input) ->
  bfs([P], #{}, Input).

bfs([], Visited, _) ->
  maps:size(Visited);
bfs([{X, Y} = C|R], Visited, {XM, YM, Map}) ->
  NV = Visited#{C => true},
  Ns = find_ns(X, Y, XM, YM),
  Cs = [P || P <- Ns, not maps:get(P, NV, false), maps:get(P, Map) =/= 9],
  bfs(R ++ Cs, NV, {XM, YM, Map}).
