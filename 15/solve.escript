#!/usr/bin/env escript

%% https://adventofcode.com/2021/day/15

-mode(compile).

main(Args) ->
  Input = read_input(),
  %%io:format("Input:~n~p~n", [Input]),
  Ans =
    case Args of
      ["2"|_] -> solve_second(Input);
      _ -> solve_first(Input)
    end,
  io:format("~p~n", [Ans]).

read_input() ->
  Lines = read_lines("~s", []),
  to_map(Lines).

read_lines(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Inputs} -> read_lines(Pat, [Inputs|Acc]);
    eof -> lists:reverse(Acc)
  end.

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

solve_first({XM, YM, _} = Input) ->
  K = {XM, YM},
  find_star(K, Input).

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

solve_second({XM, YM, _} = Input) ->
  FullInput = expand(Input),
  K = {XM * 5, YM * 5},
  find_star(K, {XM * 5, YM * 5, FullInput}).

expand({XM, YM, Map}) ->
  Seq = lists:seq(0, 4),
  Muls = [{XX, YY} || XX <- Seq, YY <- Seq, {XX, YY} =/= {0, 0}],
  ScanX = lists:seq(1, XM),
  ScanY = lists:seq(1, YM),
  FoldMuls =
    fun({MX, MY}, MulMap) ->
        Add = MX + MY,
        FoldYs =
          fun(Y, YMap) ->
              FoldXs =
                fun(X, XMap) ->
                    K = {X, Y},
                    #{K := V} = XMap,
                    VV = V + Add,
                    FV = case VV > 9 of true -> VV - 9; false -> VV end,
                    XMap#{{X + MX * XM, Y + MY * YM} => FV}
                end,
              lists:foldl(FoldXs, YMap, ScanX)
          end,
        lists:foldl(FoldYs, MulMap, ScanY)
    end,
  lists:foldl(FoldMuls, Map, Muls).

find_star(Goal, Map) ->
  HFun = fun(P) -> h(Goal, P) end,
  StartH = HFun({1,1}),
  OpenSet = gb_trees:from_orddict([{{StartH, make_ref()}, {1,1}}]),
  find_star(OpenSet, Goal, #{{1,1} => 0}, HFun, Map).

find_star(OpenSet, Goal, GScores, HFun, {XM, YM, Vs} = Map) ->
  {{_V, _}, {X, Y} = Key, NewOpenSet} = gb_trees:take_smallest(OpenSet),
  #{Key := V} = GScores,
  case Key =:= Goal of
    true -> V;
    false ->
      Neighs = find_ns(X, Y, XM, YM),
      Costs = [{PP, V + maps:get(PP, Vs)} || PP <- Neighs],
      NewAdd =
        [A || A = {PP, VV} <- Costs, VV < maps:get(PP, GScores, infinity)],
      Put = fun({PP, VV}, Acc) -> Acc#{PP => VV} end,
      NewGScores = lists:foldl(Put, GScores, NewAdd),
      F =
        fun({PP, VV}, Acc) ->
            gb_trees:insert({VV + HFun(PP), make_ref()}, PP, Acc)
        end,
      FinalOpenSet = lists:foldl(F, NewOpenSet, NewAdd),
      find_star(FinalOpenSet, Goal, NewGScores, HFun, Map)
  end.

h({XG, YG}, {X, Y}) ->       
  abs(XG - X) + abs(YG - Y).
