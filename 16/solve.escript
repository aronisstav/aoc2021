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
  io:format("~p~n", [Ans]).

read_input() ->
  {ok, [B]} = io:fread("", "~s"),
  hexstr_to_bin(B).

hexstr_to_bin(S) ->
  hexstr_to_bin(S, []).
hexstr_to_bin([], Acc) ->
  list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
  hexstr_to_bin(T, [V | Acc]);
hexstr_to_bin([X|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", lists:flatten([X,"0"])),
  hexstr_to_bin(T, [V | Acc]).

solve_first(Input) ->
  {Decoded, _} = decode(Input),
  sum_versions(Decoded).

decode(<<Vsn:3,TypeId:3,R1/bitstring>>) ->
  case type(TypeId) of
    lit ->
      {L, R} = read_literal(R1),
      {{lit, Vsn, L}, R};
    Op ->
      case R1 of
        <<0:1,Len:15,Subs:Len/bitstring,R2/bitstring>> ->
          Many = decode_many(Subs),
          {{Op, Vsn, Many}, R2};
        <<1:1,Cnt:11,R2/bitstring>> ->
          {Many, R} = decode_many(R2, Cnt),
          {{Op, Vsn, Many}, R}
      end
  end.

read_literal(Bin) ->
  read_literal(Bin, []).

read_literal(<<0:1,N:4/integer,R/bitstring>>, Acc) ->
  Fold = fun(N1, Acc1) -> Acc1 * 16 + N1 end,
  {lists:foldr(Fold, 0, [N|Acc]), R};
read_literal(<<1:1,N:4/integer,R/bitstring>>, Acc) ->
  read_literal(R, [N|Acc]).

decode_many(<<>>) ->
  [];
decode_many(Bin) ->
  {V, R} = decode(Bin),
  [V|decode_many(R)].

decode_many(Bin, Cnt) ->
  decode_many(Bin, Cnt, []).

decode_many(Bin, 0, Acc) ->
  {lists:reverse(Acc), Bin};
decode_many(Bin, N, Acc) ->
  {V, R} = decode(Bin),
  decode_many(R, N - 1, [V|Acc]).

type(0) -> sum;
type(1) -> prod;
type(2) -> min;
type(3) -> max;
type(4) -> lit;
type(5) -> gt;
type(6) -> lt;
type(7) -> eq.

sum_versions({lit, Vsn, _}) ->
  Vsn;
sum_versions({_, Vsn, Args}) ->
  Vsn + lists:sum([sum_versions(P) || P <- Args]).

solve_second(Input) ->
  {Decoded, _} = decode(Input),
  eval(Decoded).

eval({lit, _, V}) ->
  V;
eval({Type, _, ArgsRaw}) ->
  Args = [eval(A) || A <- ArgsRaw],
  case {Type, Args} of
    {sum, _} -> lists:sum(Args);
    {prod, [A1|Rest]} ->
      Prod = fun(X, V) -> X * V end,
      lists:foldl(Prod, A1, Rest);
    {min, _} -> lists:min(Args);
    {max, _} -> lists:max(Args);
    {gt, [A1, A2]} -> b2i(A1 > A2);
    {lt, [A1, A2]} -> b2i(A1 < A2);
    {eq, [A1, A2]} -> b2i(A1 =:= A2)
  end.

b2i(true) -> 1;
b2i(false) -> 0.
