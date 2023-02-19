#!/usr/bin/env escript

%% https://adventofcode.com/2021/day/24

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
  to_code(Lines, []).

read_lines(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Inputs} -> read_lines(Pat, [Inputs|Acc]);
    eof -> lists:reverse(Acc)
  end.

to_code([], Acc) ->
  lists:reverse(Acc);
to_code([["inp"], [S]|R], Acc) ->
  to_code(R, [{inp, list_to_atom(S)}|Acc]);
to_code([[Other], [S1], [S2]|R], Acc) ->
  [V1, V2] = [to_v(S) || S <- [S1, S2]],
  to_code(R, [{list_to_atom(Other), V1, V2}|Acc]).

to_v(S) ->
  try
    list_to_integer(S)
  catch
    _:_ -> list_to_atom(S)
  end.

solve_first(Input) ->
  State =
    #{ code => Input
     , input => [9,6,8,1,9,9,9,4,2,9,3,9,9,6]
     , x => 0
     , y => 0
     , z => 0
     , w => 0
     , pc => 0
     },
  exec(State).

exec(#{code := []} = State) -> State#{input => []};
exec(State) ->
  #{ code := [C|R]
   , pc := PC
   } = State,
  exec(exec(C, State#{code => R, pc => PC + 1})).

exec(Full, State) ->
  case Full of
    {inp, X} ->
      io:format("~w~n", [exec(State#{code => []})]),
      #{ input := [I|R]
       } = State,
      State#{input => R, X => I};
    {Op, A, B} ->
      VA = maps:get(A, State),
      VB = v(B, State),
      case Op of
        add -> State#{A => VA + VB};
        mul -> State#{A => VA * VB};
        'div' -> State#{A => VA div VB};
        mod -> State#{A => VA rem VB};
        eql ->
          C = if VA =:= VB -> 1; true -> 0 end,
          State#{A => C}
      end
  end.

v(V, _) when is_integer(V) -> V;
v(X, S) -> maps:get(X, S).

solve_second(Input) ->
  Input.
