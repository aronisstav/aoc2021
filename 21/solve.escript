#!/usr/bin/env escript

%% https://adventofcode.com/2021/day/21

-mode(compile).

-define(LIMIT, 999).

main(Args) ->
  Input = read_input(),
  Ans =
    case Args of
      ["2"|_] -> solve_second(Input);
      _ -> solve_first(Input)
    end,
  io:format("~w~n", [Ans]).

read_input() ->
  {4, 7}.

solve_first({P1, P2}) ->
  State =
    #{ p1 => P1 - 1
     , p2 => P2 - 1
     , s1 => 0
     , s2 => 0
     , d => 100
     , n => 0
     , p => p1
     },
  play(State).

play(State) ->
  #{ p1 := P1
   , p2 := P2
   , s1 := S1
   , s2 := S2
   , d := D
   , n := N
   , p := P
   } = State,
  %%io:format("~p~n", [State]),
  {V, ND, NN} = roll(D, N, 3),
  NState =
    State
    #{ d => ND
     , n => NN
     },
  Nxt =
    case P of
      p1 ->
        NP1 = (P1 + V) rem 10,
        NS1 = S1 + NP1 + 1,
        if NS1 > ?LIMIT -> {finish, S2 * NN};
           true ->
            FState =
              NState
              #{ p1 => NP1
               , s1 => NS1
               , p =>p2
               },
            {go, FState}
        end;
      p2 ->
        NP2 = (P2 + V) rem 10,
        NS2 = S2 + NP2 + 1,
        if NS2 > ?LIMIT -> {finish, S1 * NN};
           true ->
            FState =
              NState
              #{ p2 => NP2
               , s2 => NS2
               , p =>p1
               },
            {go, FState}
        end
    end,
  case Nxt of
    {finish, SS} -> SS;
    {go, St} -> play(St)
  end.

roll(D, N, X) ->
  roll(D, N, X, 0).

roll(D, N, 0, V) ->
  {V, D, N};
roll(D, N, X, V) ->
  DD = D rem 100 + 1,
  roll(DD, N + 1, X - 1, V + DD).

solve_second(In) ->
  42.
