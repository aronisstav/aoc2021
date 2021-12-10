#!/usr/bin/env escript

%% https://adventofcode.com/2021/day/10

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
  read_lines(Pat, []).

read_lines(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Inputs} -> read_lines(Pat, [Inputs|Acc]);
    eof -> lists:reverse(Acc)
  end.

solve_first(Input) ->
  nesting(Input).

nesting(Input) ->
  Fold =
    fun([Line], Score) ->
        Score + first_bad_score(Line)
    end,
  lists:foldl(Fold, 0, Input).

first_bad_score(Line) ->
  try
    parse(Line)
  catch
    throw:incomplete ->
      0;
    throw:{illegal, C} ->
      case C of
        $) -> 3;
        $] -> 57;
        $} -> 1197;
        $> -> 25137
      end
  end.

parse([$(|_] = L) -> parse_open(L);
parse([$[|_] = L) -> parse_open(L);
parse([${|_] = L) -> parse_open(L);
parse([$<|_] = L) -> parse_open(L);
parse(Else) -> Else.

parse_open([H|OT]) ->
  case parse(OT) of
    [] -> throw(incomplete);
    [T|R] ->
      case {H, T}  of
        {$(, $)} -> ok;
        {$[, $]} -> ok;
        {${, $}} -> ok;
        {$<, $>} -> ok;
        {_, Else} -> throw({illegal, Else})
      end,
      parse(R)
  end.

solve_second(Input) ->
  Fold =
    fun([Line], Acc) ->
        case complete_incomplete(Line) of
          {ok, S} -> [S|Acc];
          corrupt -> Acc
        end
    end,
  Scores = lists:foldl(Fold, [], Input),
  L = length(Scores),
  N = (L + 1) div 2,
  lists:nth(N, lists:sort(Scores)).

complete_incomplete(Input) ->
  try
    {ok, complete_incomplete(Input, [])}
  catch
    _ -> corrupt
  end.      

complete_incomplete([], Stack) ->
  calculate_score(Stack);
complete_incomplete([H|T], Stack) ->
  NewStack =
    case {H, Stack} of
      {$(, _} -> [H|Stack];
      {$[, _} -> [H|Stack];
      {${, _} -> [H|Stack];
      {$<, _} -> [H|Stack];
      {$), [$(|R]} -> R;
      {$], [$[|R]} -> R;
      {$}, [${|R]} -> R;
      {$>, [$<|R]} -> R;
      {S, _} -> throw({illegal, S})
    end,
  complete_incomplete(T, NewStack).

calculate_score(Stack) -> 
  Fold =
    fun(S, Acc) ->
        V =
          case S of
            $( -> 1;
            $[ -> 2;
            ${ -> 3;
            $< -> 4
          end,
        Acc * 5 + V
    end,
  lists:foldl(Fold, 0, Stack).
