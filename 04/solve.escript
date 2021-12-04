#!/usr/bin/env escript

%% https://adventofcode.com/2021/day/2

-mode(compile).

main(Args) ->
  Input = read_input(),
  %%io:format("Input:~n~p~n", [Input]),
  Ans =
    case Args of
      ["2"] -> solve_second(Input);
      _ -> solve_first(Input)
    end,
  io:format("~w~n", [Ans]).

read_input() ->
  Draw = read_draw(),
  Boards = read_boards(),
  State = make_state(Boards),
  #{draw => Draw, boards => Boards, state => State}.

read_draw() ->
  {ok, S} = io:fread("", "~s"),
  [list_to_integer(N) || N <- string:split(S, ",", all)].

read_boards() ->
  read_boards([]).

read_boards(Acc) ->
  case read_board() of
    {ok, Board} -> read_boards([Board|Acc]);
    eof -> lists:reverse(Acc)
  end.

read_board() ->
  case io:fread("", "~d") of
    {ok, [D]} -> {ok, read_board(1, 2, [D], [])};
    eof -> eof
  end.

read_board(6, _, _, Acc) ->
  lists:reverse(Acc);
read_board(Y, 6, Line, Acc) ->
  read_board(Y + 1, 1, [], [lists:reverse(Line)|Acc]);
read_board(Y, X, Line, Acc) ->
  {ok, [D]} = io:fread("", "~d"),
  read_board(Y, X + 1, [D|Line], Acc).

make_state(Boards) ->
  [[[{N, false} || N <- L] || L <- B] || B <- Boards].

solve_first(Input) ->
  find_winner(Input).

find_winner(Input) ->
  #{ draw := [N|NewDraw]
   , state := State
   } = Input,
  NewState = play(N, State),
  case has_winner(NewState) of
    false ->
      NewInput =
        Input#
        { draw => NewDraw
        , state => NewState
        },
      find_winner(NewInput);
    {ok, Board} ->
      get_score(N, Board)
  end.

play(I, State) ->
  [[[{N, Old orelse I =:= N} || {N, Old} <- L] || L <- B] || B <- State].

has_winner([]) -> false;
has_winner([Board|State]) ->
  case by_line(Board) orelse by_column(Board) of
    true -> {ok, Board};
    false -> has_winner(State)
  end.

by_line(Board) ->
  Test =
    fun(Line) ->
        lists:all(fun(X) -> X end, [S || {_, S} <- Line])
    end,
  lists:any(Test, Board).

by_column(Board) ->
  by_line(transpose(Board)).

transpose(Array) ->
  [L|_ ] = RevArray = lists:reverse(Array),
  Base = [[] || _ <- L],
  Fold =
    fun(Num, Acc) ->
        lists:zipwith(fun(H,T) -> [H|T] end, Num, Acc)
    end,
  lists:foldl(Fold, Base, RevArray).

get_score(N, Board) ->
  Unchecked =
    [[I || {I, S} <- L, S =:= false] || L <- Board],
  S = lists:sum(lists:flatten(Unchecked)),
  S * N.

solve_second(Input) ->
  find_last(Input#{last => 0}).

find_last(Input) ->
  #{ draw := Draw
   , state := State
   , last := Last
   } = Input,
  case Draw of
    [] -> Last;
    [N|NewDraw] ->
      NewState = play(N, State),
      cleanup(N, NewDraw, NewState, Last)
  end.

cleanup(N, NewDraw, NewState, Last) ->
  case has_winner(NewState) of
    false ->
      NewInput =
        #{ draw => NewDraw
         , state => NewState
         , last => Last
         },
      find_last(NewInput);
    {ok, Board} ->
      NewLast = get_score(N, Board),
      NextState = NewState -- [Board],
      cleanup(N, NewDraw, NextState, NewLast)
  end.
