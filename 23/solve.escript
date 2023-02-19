#!/usr/bin/env escript

%% https://adventofcode.com/2021/day/23

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
  A = [a, d, d, c],
  B = [d, c, b, c],
  C = [a, b, a, d],
  D = [b, a, c, b],
  {[],A,[],B,[],C,[],D,[]}.

solve_first(Input) ->
  Input.

solve_second(Input) ->
  solve(queue:from_list([{0, Input}]), infinity, #{}).

solve(Queue, Best, Seen) ->
  {Out, NQ} = queue:out(Queue),
  case Out of
    empty -> Best;
    {value, State} ->
      case win(State) of
        {ok, N} -> solve(NQ, min(N, Best), Seen);
        false ->
          {FQ, NSeen} = next(State, Seen, NQ),
          solve(FQ, Best, NSeen)
      end
  end.

win({N, {[],[a,a,a,a],[],[b,b,b,b],[],[c,c,c,c],[],[d,d,d,d],[]}}) ->
  {ok, N};
win(_) -> false.

next({V, P}, #{P := Q} = Seen, NQ) when Q =< N -> {NQ, Seen};
next({V, P} = State, Seen, NQ) ->
  NSeen = Seen#{P => V},
  From = [l, a, ab, b, bc, c, cd, d, r],
  Possible = lists:append([move(F, State) || F <- From]),
  FQ = lists:foldl(fun queue:in/2, NQ, Possible),
  {FQ, NSeen}.

move(l, {_, {[], _, _, _, _, _, _, _, _}}) -> [];
move(l, {V, {[T|R], A, AB, B, BC, C, CD, D, R} = P}) ->
  move_r(a, V, T, {R, A, AB, B, BC, C, CD, D, R}).

move_r(r, V, T, {L, A, AB, B, BC, C, CD, D, R}) ->
  case R of
    [_, _] -> [];
    _ ->
      NV =
        case R of
          [X] -> V + v(1, X) + v(2, T);
          [] -> V + v(2, T)
        end,
      [{NV, {R, A, AB, B, BC, C, CD, D, [T|R]}}]
  end;
move_r(d, V, T, {R, A, AB, B, BC, C, CD, D, R} = P) ->
  case can(T, D, V) of
    {ok, NV} ->
      [{NV, {R, A, AB, B, BC, C, CD, [T|D], R})] ++
        move(r, V +
