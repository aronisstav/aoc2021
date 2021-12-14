#!/usr/bin/env escript

%% https://adventofcode.com/2021/day/14

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
  {ok, [Base]} = io:fread("", "~s"),
  Lines = read_lines("~s -> ~s", []),
  {Base, maps:from_list([{P, R} || [P, R] <- Lines])}.

read_lines(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Inputs} -> read_lines(Pat, [Inputs|Acc]);
    eof -> lists:reverse(Acc)
  end.

solve_first({Base, Replace}) ->
  Expand = expand(10, Base, Replace),
  Freq = classify(Expand),
  [{Least, _}|_] = Sort =
    lists:sort([{F, E} || {E, F} <- maps:to_list(Freq)]),
  [{Most, _}|_] = lists:reverse(Sort),
  Most - Least.

expand(0, Base, _) -> Base;
expand(N, Base, Replace) ->
  Expanded = insert(Base, Replace, []),
  expand(N - 1, Expanded, Replace).

insert([A], _, Acc) ->
  lists:reverse([A|Acc]);
insert([A,B|R], Replace, Acc) ->
  K = [A,B],
  #{K := [I]} = Replace,
  insert([B|R], Replace, [I,A|Acc]).

classify(String) ->
  Fold =
    fun(C, Acc) ->
        Inc = fun(X) -> X + 1 end,
        maps:update_with(C, Inc, 1, Acc)
    end,
  lists:foldl(Fold, #{}, String).

solve_second({Base, Replace}) ->
  Pair = pairify(Base, #{}),
  Final = smart_expand(40, Pair, Replace),
  count(Final).

pairify([_], Map) ->
  Map;
pairify([A,B|R], Map) ->
  Inc = fun(X) -> X + 1 end,
  Upd = maps:update_with([A,B], Inc, 1, Map),
  pairify([B|R], Upd).

smart_expand(0, State, _) -> State;
smart_expand(N, State, Replace) ->
  NewState = expand_one(State, Replace),
  smart_expand(N - 1, NewState, Replace).

expand_one(State, Replace) ->
  Fold =
    fun(K, V, Acc) ->
        #{K := [I]} = Replace,
        [A,B] = K,
        Inc = fun(X) -> X + V end,
        M1 = maps:update_with([A,I], Inc, V, Acc),
        maps:update_with([I,B], Inc, V, M1)
    end,
  maps:fold(Fold, #{}, State).

count(Pairs) ->
  Fold =
    fun(K, V, Acc) ->
        [A,B] = K,
        Inc = fun(X) -> X + V end,
        M1 = maps:update_with([A], Inc, V, Acc),
        maps:update_with([B], Inc, V, M1)
    end,
  Singles = maps:fold(Fold, #{}, Pairs),
  Halve = fun(_, V) -> V div 2 + V rem 2 end,
  Freq = maps:map(Halve, Singles),
  [{Least, _}|_] = Sort =
    lists:sort([{F, E} || {E, F} <- maps:to_list(Freq)]),
  [{Most, _}|_] = lists:reverse(Sort),
  Most - Least.

            
