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
  {ok, File} = io:fread("", "~s"),
  {ok, T} = file:consult(File),
  T.

solve_first([F|In]) ->
  Fold =
    fun(N, Acc) ->
        reduce([Acc,N])
    end,
  Final = lists:foldl(Fold, F, In),
  magnitude(Final).

reduce(N) ->
  case reduce(N, 0) of
    {stable, N} -> N;
    {explode, _, _, Q} -> reduce(Q);
    {split, Q} -> reduce(Q)
  end.

reduce(A, _) when is_integer(A), A > 9 ->
  DA = A div 2,
  RA = A rem 2,
  {split, [DA, DA + RA]};
reduce(A, _) when is_integer(A) ->
  {stable, A};
reduce([A, B], 4) ->
  {explode, A, B, 0};
reduce([A, B], D) ->
  case {reduce(A, D + 1), reduce(B, D + 1)} of
    {{explode, L, R, V}, _} ->
      {NR, NB} = add_l(R, B),
      {explode, L, NR, [V, NB]};
    {_, {explode, L, R, V}} ->
      {NL, NA} = add_r(L, A),
      {explode, NL, R, [NA, V]};
    {{split, Q}, _} ->
      {split, [Q, B]};
    {_, {split, Q}} ->
      {split, [A, Q]};
    {_, _} ->
      {stable, [A, B]}
  end.

add_l(-1, Q) -> {-1, Q};
add_l(V, B) when is_integer(B) -> {-1, V + B};
add_l(V, [A, B]) ->
  {-1, Q} = add_l(V, A),
  {-1, [Q, B]}.

add_r(-1, Q) -> {-1, Q};
add_r(V, B) when is_integer(B) -> {-1, V + B};
add_r(V, [A, B]) ->
  {-1, Q} = add_r(V, B),
  {-1, [A, Q]}.

magnitude(V) when is_integer(V)->
  V;
magnitude([A, B]) ->
  3 * magnitude(A) + 2 * magnitude(B).

solve_second(In) ->
  Pairs = [{A, B} || A <- In, B <- In, A =/= B],
  Fold =
    fun({A, B}, Max) ->
        max(magnitude(reduce([A, B])), max(magnitude(reduce([B, A])), Max))
    end,
  lists:foldl(Fold, 0, Pairs).
