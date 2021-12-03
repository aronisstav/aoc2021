#!/usr/bin/env escript

%% https://adventofcode.com/2021/day/2

-mode(compile).

main(Args) ->
  Input = read_input(),
  %% io:format("Input:~n~p~n", [Input]),
  Ans =
    case Args of
      ["2"] -> solve_second(Input);
      _ -> solve_first(Input)
    end,
  io:format("~w~n", [Ans]).

read_input() ->
  [S || [S] <- read_list("~s")].

read_list(Pat) -> read_list(Pat, []).

read_list(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Res} -> read_list(Pat, [Res|Acc]);
    eof -> lists:reverse(Acc)
  end.

solve_first(Input) ->
  Transposed = transpose(Input),
  MCB = most_common_bits(Transposed),
  LCB = complement(MCB),
  to_num(MCB) * to_num(LCB).

transpose(Array) ->
  [L|_ ] = RevArray = lists:reverse(Array),
  Base = [[] || _ <- L],
  Fold =
    fun(Num, Acc) ->
        lists:zipwith(fun(H,T) -> [H|T] end, Num, Acc)
    end,
  lists:foldl(Fold, Base, RevArray).

most_common_bits(Array) ->
  Fold =
    fun(Line, Acc) ->
        MCB = most_common_bit(Line),
        [MCB|Acc]
    end,
  lists:foldr(Fold, [], Array).

most_common_bit(Line) ->
  Fold =
    fun($0, {Z, O}) -> {Z + 1, O};
       ($1, {Z, O}) -> {Z, O + 1}
    end,
  case lists:foldl(Fold, {0, 0}, Line) of
    {MZ, O} when MZ > O -> $0;
    {Z, MO} when MO > Z -> $1;
    _ -> $1
  end.

complement(Line) ->
  [c(D) || D <- Line].

c($0) -> $1;
c($1) -> $0.

to_num(Line) ->
  Fold =
    fun($0, N) -> 2 * N;
       ($1, N) -> 2 * N + 1
    end,
  lists:foldl(Fold, 0, Line).   

solve_second(Input) ->
  Oxygen = filter(Input, most),
  CO2 = filter(Input, least),
  to_num(Oxygen) * to_num(CO2).

filter(Array, Mode) ->
  filter(Array, Mode, 1).

filter([X], _, _) ->
  X;
filter(Array, Mode, Pos) ->
  Labeled = label(Array, Pos),
  Labels = [L || {L, _} <- Labeled],
  MCL = most_common_bit(Labels),
  Criterion =
    case Mode of
      most -> MCL;
      least -> c(MCL)
    end,
  NewArray = [Entry || {L, Entry} <- Labeled, L =:= Criterion],
  filter(NewArray, Mode, Pos + 1).

label(Array, Pos) ->
  Fold =
    fun(Line, Acc) ->
        Acc ++ [{lists:nth(Pos, Line), Line}]
    end,
  lists:foldl(Fold, [], Array).
