#!/usr/bin/env escript

%% https://adventofcode.com/2021/day/19

-mode(compile).

main(Args) ->
  Input = read_input(),
  Ans =
    case Args of
      ["2"|_] -> solve_second(Input);
      _ -> solve_first(Input)
    end,
  io:format("~p~n", [Ans]).

read_input() ->
  Lines = read_lines("~s", []),
  to_input(Lines).

read_lines(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Inputs} -> read_lines(Pat, [Inputs|Acc]);
    eof -> lists:reverse(Acc)
  end.

to_input(Lines) ->
  [["---"], ["scanner"], ["0"], ["---"]|Rest] = Lines,
  to_input(Rest, #{}, #{}, 0, 0).

to_input([], Input, Probes, N, _) ->
  Input#{N => Probes};
to_input([[Line]|Rest], Input, Probes, N, I) ->
  case Line of
    "---" ->
      [["scanner"], [NS], ["---"]|RR] = Rest,
      NN = list_to_integer(NS),
      NewInput = Input#{N => Probes},
      to_input(RR, NewInput, #{}, NN, 0);
    Coords ->
      XYZ = [list_to_integer(W) || W <- string:split(Coords, ",", all)],
      NewProbes = Probes#{I => XYZ},
      to_input(Rest, Input, NewProbes, N, I + 1)
  end.

solve_first(Probes) ->
  Ds = dists(Probes),
  Map = match(Probes, Ds),
  distinct(Map).

dists(Probes) ->
  Fold =
    fun(K, V, Acc) ->
        Acc#{K => to_dists(V)}
    end,
  maps:fold(Fold, #{}, Probes).

to_dists(V) ->
  K = maps:keys(V),
  Fold =
    fun({A, B}, Acc) ->
        #{ A := AXYZ
         , B := BXYZ
         } = V,
        Acc#{{A, B} => to_dists(AXYZ, BXYZ)}
    end,
  Dists = lists:foldl(Fold, #{}, [{A, B} || A <- K, B <- K, A =/= B, A > B]),
  #{dmap => dmap(Dists), dists => Dists}.

dmap(Dists) ->
  Fold =
    fun(K, V, Acc) ->
        Upd = fun(R) -> [K|R] end,
        S = lists:sum([abs(P) || P <- V]),
        maps:update_with(S, Upd, [K], Acc)
    end,
  maps:fold(Fold, #{}, Dists).

to_dists([A, B, C], [X, Y, Z]) ->
  [A - X, B - Y, C - Z].

match(Probes, Dists) ->
  Rels = find_rels(Dists, Probes),
  final_map(Probes, Rels).

find_rels(Dists, Probes) ->
  ZP = {{0, 0, 0}, {xp, yp, z}},
  find_rels(#{0 => ZP}, Dists, Probes).

find_rels(Rels, Dists, Probes) ->
  K = maps:keys(Dists),
  Rs = maps:keys(Rels),
  Unknown = K -- Rs,
  case Unknown =:= [] of
    true -> Rels;
    false ->
      find_rels(Unknown, Dists, Probes, Rels)
  end.

find_rels([], _, _, Rels) -> Rels;
find_rels([U|R], Dists, Probes, Rels) ->
  Known = maps:keys(Rels),
  NewRels =
    case scan_known(U, Known, Dists, Probes, Rels) of
      {ok, P} -> Rels#{U => P};
      false -> Rels
    end,
  find_rels(R, Dists, NewRels).

scan_known(_, [], _, _, _) -> false;
scan_known(U, [K|R], Dists, Probes, Rels) ->
  RelatesTo =
    case relates_to(K, U, Dists) of
      {ok, RelInfo} -> place(U, K, RelInfo, Probes, Rels);
      false -> false
    end,
  case RelatesTo =:= false of
    true -> scan_known(U, R, Dists, Probes, Rels);
    false -> RelatesTo
  end.

relates_to(K, U, Dists) ->
  #{ K := KDists
   , U := UDists
   } = Dists,
  relate_to(KDists, UDists).

relate_to(KFDists, UFDists) ->
  #{dmap := UDMap, dists := UDists} = UFDists,
  #{dmap := KDMap, dists := KDists} = KFDists,
  It = maps:iterator(KDMap),
  find_matching_pair(It, KDists, UDMap, UDists).

find_matching_pair(It, KDists, UDMap, UDists) ->
  case maps:next(It) of
    none -> false;
    {K, KVs, NIt} ->
      Match =
        case maps:get(K, UDMap, none) of
          none -> false;
          UVs -> check_cands(KVs, KDists, UVs, UDists)
        end,
      case Match =:= false of
        true -> find_matching_pair(NIt, KDists, UDMap, UDists);
        false -> Match
      end
  end.

check_cands([], _, _, _) -> false;
check_cands([P|Rs], KDists, UVs, UDists) ->
  #{P := Ds} = KDists,
  Match = check_ds(Ds, UVs, UDists),
  case Match of
    {ok, U} -> {ok, {U, P}};
    false -> check_cands(Rs, KDists, UVs, UDists)
  end.

check_ds(_, [], _) -> false;
check_ds(Ds, [U|R], UDists) ->
  #{U := Us} = UDists,
  case equi(Ds, Us) of
    true -> {ok, U};
    false -> check_ds(Ds, R, UDists)
  end.

equi(Ds, Us) ->
  SDs = lists:sort([abs(D) || D <- Ds]),
  SUs = lists:sort([abs(U) || U <- Us]),
  SDs =:= SUs.

place(U, K, {{UA, UB}, {KA, KB}}, Probes, Rels) ->
  #{ K := KProbes
   , U := UProbes
   } = Probes,
  Axes = [[X, Y, z] || X <- [xp, xn], Y <- [yp, yn]],
  Orientations =
    [{X, Y, Z, Order} ||
      X <- Axes, Y <- Axes, Z <- Axes,
      Order <- [match, cross]
      X =/= Y, X =/= Z, Y =/= Z
    ],



distinct(Map) ->
  Map.

solve_second(In) ->
  solve_first(In).
