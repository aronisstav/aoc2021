#!/usr/bin/env escript

%% https://adventofcode.com/2021/day/8

-mode(compile).

main(Args) ->
  Input = read_input(),
  %% io:format("Input:~n~p~n", [Input]),
  Ans =
    case Args of
      ["2"|_] -> solve_second(Input);
      _ -> solve_first(Input)
    end,
  io:format("~w~n", [Ans]).

read_input() ->
  Pat =
    ["~s " || _ <- lists:seq(1, 10)] ++
    ["| "] ++
    ["~s " || _ <- lists:seq(1, 4)],
  Lines = read_lines(lists:flatten(Pat), []),
  [lists:split(10, L) || L <- Lines].

read_lines(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Inputs} -> read_lines(Pat, [Inputs|Acc]);
    eof -> lists:reverse(Acc)
  end.

solve_first(Input) ->
  count_easy(Input).

count_easy(Input) ->
  Fold =
    fun({_, Out}, C) ->
        C + length([L || L <- [length(O) || O <- Out], L =/= 5, L =/= 6])
    end,
  lists:foldl(Fold, 0, Input).

solve_second(Input) ->
  lists:sum([decode(Case) || Case <- Input]).

decode({In, Out}) ->
  decode([lists:sort(S) || S <- In], [lists:sort(S) || S <- Out]).

decode(In, Out) ->
  Map = find_map(In),
  to_num([maps:get(D, Map) || D <- Out]).

find_map(In) ->
  CF     = find_cf(In),
  A      = find_a(CF, In),
  BD     = find_bd(CF, In),
  {C, G} = find_c_g(CF, BD, A, In),
  D      = find_d(CF, A, G, In),
  [B]    = BD -- [D],
  [F]    = CF -- [C],
  E      = find_e([A, B, C, D, F, G], In),
  make_map(A, B, C, D, E, F, G).

find_cf(In) ->
  [One] = [CF || CF <- In, length(CF) =:= 2],
  One.

find_a(CF, In) ->
  [Seven] = [ACF || ACF <- In, length(ACF) =:= 3],
  [A] = Seven -- CF,
  A.

find_bd(CF, In) ->
  [Four] = [BCDF || BCDF <- In, length(BCDF) =:= 4],
  Four -- CF.

find_c_g(CF, BD, A, In) ->
  ABD = lists:sort([A|BD]),
  ABDers = [C || C <- In, length(ABD -- C) =:= 0],
  [Five] = [ABDFG || ABDFG <- ABDers, length(ABDFG -- ABD) =:= 2],
  [G] = (Five -- ABD) -- CF,
  [C] = CF -- Five,
  {C, G}.

find_d(CF, A, G, In) ->
  ACFG = lists:sort([A,G|CF]),
  ACFGers = [C || C <- In, length(ACFG -- C) =:= 0],
  [Three] = [ACDFG || ACDFG <- ACFGers, length(ACDFG -- ACFG) =:= 1],
  [D] = Three -- ACFG,
  D.

find_e(ABCDFG, In) ->
  [Eight] = [ABCDEFG || ABCDEFG <- In, length(ABCDEFG) =:= 7],
  [E] = Eight -- ABCDFG,
  E.

make_map(A, B, C, D, E, F, G) ->
  List =
    [{lists:sort(M), N} ||
      {N, M} <-
        [ {0, [A, B, C,    E, F, G]}
        , {1, [      C,       F   ]}
        , {2, [A,    C, D, E,    G]}
        , {3, [A,    C, D,    F, G]}
        , {4, [   B, C, D,    F   ]}
        , {5, [A, B,    D,    F, G]}
        , {6, [A, B,    D, E, F, G]}
        , {7, [A,    C,       F   ]}
        , {8, [A, B, C, D, E, F, G]}
        , {9, [A, B, C, D,    F, G]}
        ]],
  maps:from_list(List).

to_num(Ds) ->
  Fold = fun(D, A) -> 10 * A + D end,
  lists:foldl(Fold, 0, Ds).
