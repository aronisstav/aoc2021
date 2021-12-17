#!/usr/bin/env escript

%% https://adventofcode.com/2021/day/16

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
  {ok, [XL,XH,YL,YH]} = io:fread("", "target area: x=~d..~d, y=~d..~d"),
  #{xl => XL, xh => XH, yl => YL, yh => YH}.

solve_first(In) ->
  #{ yl := YL
   , yh := YH
   } = In,
  lists:max([find(N, YL, YH) || N <- lists:seq(1,1000)]).

find(N, YL, YH) when N > 0 ->
  Top = top(N),
  case hit(Top, YL, YH, 1) of
    true -> Top;
    false -> -1
  end;
find(N, YL, YH) ->
  case hit(0, YL, YH, abs(N)) of
    true -> 0;
    false -> -1
  end.

top(N) -> N * (N + 1) div 2.

hit(H, YL, YH, _) when H =< YH, H >= YL -> true;
hit(H, YL, _, _) when H < YL -> false;
hit(H, YL, YH, V) ->
  hit(H - V, YL, YH, V + 1).

solve_second(In) ->
  Fold =
    fun({X, Y}, Acc) ->
        case full_hit(X, Y, In) of
          true -> Acc + 1;
          false -> Acc
        end
    end,
  ScanX = lists:seq(1, 200),
  ScanY = lists:seq(-200, 200),
  lists:foldl(Fold, 0, [{X, Y} || X <- ScanX, Y <- ScanY]).

full_hit(X, Y, In) ->
  #{ yl := YL
   , yh := YH
   , xl := XL
   , xh := XH
   } = In,
  full_hit(0, 0, X, Y, XH, XL, YH, YL).

full_hit(X, _,  _,  _, XH,  _,  _,  _) when X > XH -> false;
full_hit(_, Y,  _,  _,  _,  _,  _, YL) when Y < YL -> false;
full_hit(X, Y,  _,  _, XH, XL, YH, YL)
  when XL =< X, X =< XH, YL =< Y, Y =< YH -> true;
full_hit(X, Y, VX, VY, XH, XL, YH, YL) ->
  XX = X + VX,
  YY = Y + VY,
  VXX = if VX > 0 -> VX - 1; true -> 0 end,
  VYY = VY - 1,
  full_hit(XX, YY, VXX, VYY, XH, XL, YH, YL).
