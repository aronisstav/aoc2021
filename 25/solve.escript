#!/usr/bin/env escript

%% https://adventofcode.com/2021/day/25

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
              {X + 1, IMap#{{X, Y} => S}}
          end,
        {L, OMap} = lists:foldl(IFold, {1, Map}, Line),
        {Y + 1, L, OMap}
    end,
  {YM, XM, OMap} = lists:foldl(Fold, {1, 0, #{}}, Lines),
  {XM - 1, YM - 1, OMap}.

solve_first(Input) ->
  stabilize(Input, 0).

stabilize(State, N) ->
  Evolved = evolve(State),
  case Evolved =:= State of
    true -> N;
    false ->stabilize(Evolved, N + 1)
  end.

evolve(State) ->
  move(south, move(east, State)).

move(Dir, {XM, YM, Map}) ->
  ScanX = lists:seq(1, XM),
  ScanY = lists:seq(1, YM),
  FoldY =
    fun(Y, AccY) ->
        FoldX =
          fun(X, AccX) ->
              Cur = {X, Y},
              Nxt = next(Dir, Cur, {XM, YM}),
              At = maps:get(Cur, Map),
              NxtAt = maps:get(Nxt, Map),
              case {Dir, At, NxtAt} of
                {east, $>, $.} ->
                  AccX#{Cur => $., Nxt => $>};
                {south, $v, $.} ->
                  AccX#{Cur => $., Nxt => $v};
                {_, $., _} ->
                  AccX#{Cur => maps:get(Cur, AccX, $.)};
                {_, C, _} ->
                  AccX#{Cur => C}
              end
          end,
        lists:foldl(FoldX, AccY, ScanX)
    end,
  {XM, YM, lists:foldl(FoldY, #{}, ScanY)}.

next(east, {X, Y}, {XM, _}) ->
  {(X rem XM) + 1, Y};
next(south, {X, Y}, {_, YM}) ->
  {X, (Y rem YM) + 1}.

solve_second(_Input) ->
  42.
