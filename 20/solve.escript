#!/usr/bin/env escript

%% https://adventofcode.com/2021/day/18
%% Requires input to be modified to have a '.' at the end of each line.

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
  {ok, [IEAS]} = io:fread("", "~s"),
  IEA = to_iea(IEAS),
  Image = to_map(read_lines("~s", [])),
  #{iea => IEA, img => Image, min => {1,1}, frm => $.}.

to_iea(IEAS) ->
  Fold = fun(C, {N, Acc}) -> {N + 1, Acc#{N => C}} end,
  {512, IEA} = lists:foldl(Fold, {0, #{}}, IEAS),
  IEA.

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

read_lines(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Inputs} -> read_lines(Pat, [Inputs|Acc]);
    eof -> lists:reverse(Acc)
  end.

solve_first(In) ->
  count_lit(evolve(In, 2)).

evolve(State, 0) -> State;
evolve(State, N) -> evolve(evolve(State), N - 1).  

evolve(State) ->
  #{ iea := IEA
   , img := {XH, YH, Image}
   , min := {XL, YL}
   , frm := C
   } = State,
  %%print(State),
  {NXH, NYH, NXL, NYL} = {XH + 1, YH + 1, XL - 1, YL - 1},
  ScanY = lists:seq(NYL, NYH),
  ScanX = lists:seq(NXL, NXH),
  FoldY =
    fun(Y, YAcc) ->
        FoldX =
          fun(X, XAcc) -> XAcc#{{X, Y} => evolve({X, Y}, Image, IEA, C)} end,
        lists:foldl(FoldX, YAcc, ScanX)
    end,
  NImage = lists:foldl(FoldY, #{}, ScanY),
  #{ iea => IEA
   , img => {NXH, NYH, NImage}
   , min => {NXL, NYL}
   , frm => cmp(C)
   }.

evolve({X, Y}, Map, IEA, D) ->
  Ps = [{X + XX, Y + YY} || YY <- [-1,0,1], XX <- [-1,0,1]],
  S = [maps:get(P, Map, D) || P <- Ps],
  Fold = fun(C, Acc) -> v(C) + 2 * Acc end,
  L = lists:foldl(Fold, 0, S),
  maps:get(L, IEA).

v($.) -> 0;
v($#) -> 1.

cmp($#) -> $.;
cmp($.) -> $#.

count_lit(State) ->
  %%print(State),
  #{ img := {_, _, Image}
   } = State,
  Fold = fun(_, C, Acc) -> v(C) + Acc end,
  maps:fold(Fold, 0, Image).

%% print(State) ->
%%   #{ img := {XH, YH, Image}
%%    , min := {XL, YL}
%%    } = State,
%%   ScanY = lists:seq(YL, YH),
%%   ScanX = lists:seq(XL, XH),
%%   FoldY =
%%     fun(Y) ->
%%         FoldX =
%%           fun(X) -> io:format("~c", [maps:get({X,Y}, Image)]) end,
%%         lists:foreach(FoldX, ScanX),
%%         io:format("~n")
%%     end,
%%   lists:foreach(FoldY, ScanY),
%%   io:format("~n").

solve_second(In) ->
  count_lit(evolve(In, 50)).
