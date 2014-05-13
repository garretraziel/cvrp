-module(vrpserial).
-export([main/0, main/1, main/7]).

-record(vrpProblem, {nodes,
                     distancemap,
                     depot,
                     capacity,
                     overcapcoef,
                     popcount,
                     mutateprob,
                     tournament}).

main([FilenameArg, CarNumArg, PopCountArg, OverCapCoefArg,
      IterationsArg, MutateProbArg, TournamentArg]) ->
    Filename = atom_to_list(FilenameArg),
    Cars = list_to_integer(atom_to_list(CarNumArg)),
    PopCount = list_to_integer(atom_to_list(PopCountArg)),
    OverCapCoef = list_to_integer(atom_to_list(OverCapCoefArg)),
    Iterations = list_to_integer(atom_to_list(IterationsArg)),
    MutateProb = list_to_integer(atom_to_list(MutateProbArg)),
    Tournament = list_to_integer(atom_to_list(TournamentArg)),
    main(Filename, Cars, PopCount, OverCapCoef, Iterations, MutateProb, Tournament),
    init:stop();
main(_) ->
    main().

main() ->
    io:format("ERROR: You must run this program with seven arguments,~n"),
    io:format("- first is the name of the file with VCRP task,~n"),
    io:format("- second is the number of cars to use,~n"),
    io:format("- third is the number of individuals in population,~n"),
    io:format("- fourth is coefficient of over-capacity penalty,~n"),
    io:format("- fifth is number of iterations,~n"),
    io:format("- sixth is probability of mutation, 1% .. 100%,~n"),
    io:format("- seventh is number of rounds of tournament selection.~n"),
    init:stop().

main(Filename, Cars, PopCount, OverCapCoef, Iterations, MutateProb, Tournament) ->
    statistics(runtime),
    statistics(wall_clock),
    random:seed(erlang:now()),

    {ok, Bin} = file:read_file(Filename),
    Parsed = parse_bin(Bin),
    print_info(Parsed),
    {Capacity, Nodes, Depot} = get_content(Parsed),
    DistanceMap = compute_distance_map(Nodes),
    VRP = #vrpProblem{nodes=Nodes, distancemap=DistanceMap,
                      depot=Depot, capacity=Capacity, overcapcoef=OverCapCoef,
                      popcount=PopCount, mutateprob=MutateProb,
                      tournament=Tournament},
    InitPopulation = create_init_population(PopCount, Cars, lists:delete(Depot, proplists:get_keys(Nodes))),
    
    io:format("initial population created~n"),

    Population = [{fitness(X, VRP), X} || X <- InitPopulation],

    {BestFit, BestSolution} = generations_iterate(Population, Iterations, VRP),
    
    io:format("Best: ~p, fitness: ~p~n", [BestSolution, BestFit]),

    {_, Time1} = statistics(runtime),
    {_, Time2} = statistics(wall_clock),

    U1 = Time1/1000,
    U2 = Time2/1000,
    io:format("Code time ~p (~p) seconds~n", [U1, U2]),

    {BestSolution, VRP}.

generations_iterate(Population, Count, VRP) ->
    Sorted = lists:keysort(1, Population),
    generations_iterate(Sorted, Count, VRP, 0).

generations_iterate([H|_], Total, _, Total) ->
    H;
generations_iterate(Population, Total, VRP = #vrpProblem{popcount=PopCount,
                                                         mutateprob=MutateProb,
                                                         tournament=Tournament},
                    Count) ->
    SelectorLength = round(PopCount/3),

    NewIndividuals = [tournament_selector(Population, Tournament, PopCount, MutateProb) || _ <- lists:seq(1, SelectorLength)],

    Flattened = lists:foldl(fun ({X,Y}, Acc) -> [X,Y|Acc] end, [], NewIndividuals),
    Children = [{fitness(X, VRP), X} || X <- Flattened],

    {Elite, _} = lists:split(length(Population)-length(Children), Population),
    Replaced = lists:append(Elite, Children),
    
    NewValues = lists:keysort(1, Replaced),

    {BestFit, _} = hd(NewValues),
    io:format("~p. generation: Avg fitness: ~p, Best: ~p~n", [Count, round(average_fitness(NewValues)), BestFit]),
    generations_iterate(NewValues, Total, VRP, Count+1).

parse_bin(Bin) ->
    [string:strip(X) || X <- string:tokens(binary_to_list(Bin), "\r\n")].

print_info(Parsed) ->
    lists:map(fun(X) -> io:format("~s~n", [X]) end, lists:takewhile(fun(X) -> X /= "NODE_COORD_SECTION" end, Parsed)),
    io:format("...~n").

get_content(Parsed) ->
    [CapacityWhole|_] = lists:dropwhile(fun(X) -> string:substr(X, 1, length("CAPACITY")) /= "CAPACITY" end, Parsed),
    [_|CapacityStr] = lists:dropwhile(fun(X) -> X /= $: end, CapacityWhole),
    Capacity = list_to_integer(string:strip(CapacityStr)),

    [_|Data] = lists:dropwhile(fun(X) -> X /= "NODE_COORD_SECTION" end, Parsed),
    {NodesStr, [_|Rest]} = lists:splitwith(fun(X) -> X /= "DEMAND_SECTION" end, Data),
    {DemandsStr, [_,DepotStr|_]} = lists:splitwith(fun(X) -> X /= "DEPOT_SECTION" end, Rest),

    DemandsMap = [list_to_tuple(lists:map(fun(X) -> {I, _} = string:to_integer(X), I end, string:tokens(Node, " ")))
                  || Node <- DemandsStr],

    NodesList = [lists:map(fun(X) -> {I, _} = string:to_integer(X), I end, string:tokens(Node, " "))
                 || Node <- NodesStr],
    Nodes = lists:map(fun(X) -> {I, A, B} = list_to_tuple(X), {_, Demand} = lists:keyfind(I, 1, DemandsMap),
                                {I, {A, B, Demand}} end, NodesList),

    {Depot, _} = string:to_integer(DepotStr),

    {Capacity, Nodes, Depot}.

compute_distance_map(Nodes) ->
    [{{A, B}, math:sqrt((X1-X2)*(X1-X2) + (Y1-Y2)*(Y1-Y2))} || {A, {X1, Y1, _}} <- Nodes, {B, {X2, Y2, _}} <- Nodes].

%% chromozom vypada jako: [[], [], [], [], []]
fitness(Chromosome, #vrpProblem{nodes=Nodes, distancemap=DistanceMap,
                                depot=Depot, capacity=Capacity, overcapcoef=CapCoef})
  when length(Chromosome) > 0 ->
    fitness(Chromosome, Nodes, DistanceMap, Depot, Capacity, CapCoef, 0, 0);
fitness(_, _) ->
    erlang:error(bad_chromosome).

fitness([], _, _, _, _, CapCoef, Cost, OverCapacity) when OverCapacity > 0 ->
    Cost+CapCoef*OverCapacity;
fitness([], _, _, _, _, _, Cost, _) ->
    Cost;
fitness([H|T], Nodes, DistanceMap, Depot, Capacity, CapCoef, Cost, OverCapacity) ->
    {ActualCost, ActualCap} = fitness_dist([Depot|H]++[Depot], Nodes, DistanceMap),
    ActualOverCap = if ActualCap > Capacity -> (ActualCap - Capacity); true -> 0 end,
    fitness(T, Nodes, DistanceMap, Depot, Capacity, CapCoef, Cost+ActualCost, OverCapacity+ActualOverCap).

fitness_dist([], _, _) ->
    {0, 0};
fitness_dist([H|T], Nodes, DistanceMap) ->
    fitness_dist(H, T, Nodes, DistanceMap, 0, 0).

fitness_dist(H, [], Nodes, _, Cost, CapUsed) ->
    {_, {_, _, Cap}} = lists:keyfind(H, 1, Nodes),
    {round(Cost), CapUsed+Cap};
fitness_dist(Last, [H|T], Nodes, DistanceMap, Cost, CapUsed) ->
    {_, {_, _, Cap}} = lists:keyfind(Last, 1, Nodes),
    {_, Distance} = lists:keyfind({Last, H}, 1, DistanceMap),
    fitness_dist(H, T, Nodes, DistanceMap, Cost+Distance, CapUsed+Cap).

average_fitness(Values) ->
    lists:foldl(fun({V, _}, Acc) -> Acc + V end, 0, Values)/length(Values).

shuffle(List) ->
    [X || {_, X} <- lists:sort([{random:uniform(), N} || N <- List])].

create_init_population(0, _, _) ->
    [];
create_init_population(Count, CarCount, Nodes) ->
    create_init_population(Count, CarCount, Nodes, []).

create_init_population(0, _, _, List) ->
    List;
create_init_population(Count, CarCount, Nodes, List) ->
    Shuffled = shuffle(Nodes),
    Segments = [random:uniform(length(Nodes)) || _ <- lists:seq(1, CarCount)],
    Cutted = cut_list(Shuffled, Segments),
    create_init_population(Count-1, CarCount, Nodes, [Cutted|List]).

cut_list(List, Segments) ->
    cut_list(List, Segments, []).

cut_list([], [], Acc) ->
    lists:reverse(Acc);
cut_list(_, [], _) ->
    erlang:error(bad_segment_count);
cut_list([], [_|T], Acc) ->
    cut_list([], T, [[]|Acc]);
cut_list(List, [_], Acc) ->
    lists:reverse([List|Acc]);
cut_list(List, [H|T], Acc) when H > length(List) ->
    cut_list([], T, [List|Acc]);
cut_list(List, [H|T], Acc) ->
    {First, Others} = lists:split(H, List),
    cut_list(Others, T, [First|Acc]).

tournament_selector(Individuals, TournamentCount, Length, MProb) ->
    tournament_selector(Individuals, TournamentCount, Length, MProb, none, none).

tournament_selector(_, 0, _, _, none, none) ->
    erlang:error(bad_tournament_count);
tournament_selector(Individuals, 0, _, MProb, I, J) ->
    {_, RepA} = lists:nth(I, Individuals),
    {_, RepB} = lists:nth(J, Individuals),
    {Ca, Cb} = crosser(RepA, RepB),
    case random:uniform(100) =< MProb of
        true ->
            {mutate(Ca), mutate(Cb)};
        false ->
            {Ca, Cb}
    end;
tournament_selector(Individuals, C, Length, MProb, I, J) ->
    First = random:uniform(Length),
    Second = random:uniform(Length),
    MinI = if First < I ->
                   First;
              true ->
                   I
           end,
    MinJ = if Second < J ->
                   Second;
              true ->
                   J
           end,
    tournament_selector(Individuals, C-1, Length, MProb, MinI, MinJ).

crosser(RepA, RepB) ->
    crossover(RepA, RepB).

crossover(ChromosomeA, ChromosomeB) ->
    IndexTable = lists:sort(lists:flatten(ChromosomeA)),
    IndexChromA = create_ichromosome(ChromosomeA, IndexTable),
    IndexChromB = create_ichromosome(ChromosomeB, IndexTable),
    SplitIndex = random:uniform(length(IndexChromA)),
    {FirstA, SecondA} = lists:split(SplitIndex, IndexChromA),
    {FirstB, SecondB} = lists:split(SplitIndex, IndexChromB),
    FirstIndexChrom = from_ichrom(FirstA ++ SecondB, IndexTable),
    SecondIndexChrom = from_ichrom(FirstB ++ SecondA, IndexTable),
    ChildA = unflatten_by(FirstIndexChrom, ChromosomeA),
    ChildB = unflatten_by(SecondIndexChrom, ChromosomeB),
    {ChildA, ChildB}.

create_ichromosome(Chromosome, IndexTable) ->
    create_ichromosome(Chromosome, IndexTable, []).

create_ichromosome([], _, Acc) ->
    lists:reverse(Acc);
create_ichromosome([[]|T], IndexTable, Acc) ->
    create_ichromosome(T, IndexTable, Acc);
create_ichromosome([[H|TC]|TR], IndexTable, Acc) ->
    Index = index_of(H, IndexTable),
    IndexTableChanged = lists:delete(H, IndexTable),
    create_ichromosome([TC|TR], IndexTableChanged, [Index|Acc]).

from_ichrom(IndexChromosome, IndexTable) ->
    from_ichrom(IndexChromosome, IndexTable, []).

from_ichrom([], _, Acc) ->
    lists:reverse(Acc);
from_ichrom([H|T], IndexTable, Acc) ->
    Value = lists:nth(H, IndexTable),
    IndexTableChanged = lists:delete(Value, IndexTable),
    from_ichrom(T, IndexTableChanged, [Value|Acc]).

index_of(Item, List) ->
    index_of(Item, List, 1).

index_of(_, [], _) ->
    erlang:error(cannot_find_value);
index_of(Item, [Item|_], I) ->
    I;
index_of(Item, [_|T], I) ->
    index_of(Item, T, I+1).

unflatten_by(FlatList, Chromosome) ->
    unflatten_by(FlatList, Chromosome, []).

unflatten_by([], _, Acc) ->
    lists:reverse(Acc);
unflatten_by(FlatList, [H|T], Acc) ->
    {First, Second} = lists:split(length(H), FlatList),
    unflatten_by(Second, T, [First|Acc]).

mutate(Chromosome) ->
    I = random:uniform(length(Chromosome)),
    J = random:uniform(length(Chromosome)),
    case random:uniform(2) of
        1 ->
            if I == J ->
                    Splited = lists:split(I-1, Chromosome),
                    mutate(Chromosome, Splited);
               true ->
                    As = lists:split(I-1, Chromosome),
                    B = lists:nth(J, Chromosome),
                    mutate(Chromosome, As, B, J)
            end;
        2 ->
            Length = length(lists:nth(I, Chromosome)),
            if Length == 0 ->
                    mutate(Chromosome);
               true ->
                    K = random:uniform(Length),
                    mutate_give(Chromosome, I, J, K)
            end
    end.

mutate(Chromosome, {_, [N|_]}) when length(N) == 0 ->
    mutate(Chromosome);
mutate(_, {NPrev, [N|NPost]}) ->
    I = random:uniform(length(N)),
    J = random:uniform(length(N)),
    NPrev ++ [swap(N, I, J) | NPost].

mutate(Chromosome, {_, [A|_]}, B, _) when length(A) == 0; length(B) == 0 ->
    mutate(Chromosome);
mutate(_, {APrev, [A|APost]}, B, BIndex) ->
    I = random:uniform(length(A)),
    J = random:uniform(length(B)),
    {N, M} = swap_inside(A, I, B, J),
    LT = APrev ++ [N|APost],
    {List3, [_|List4]} = lists:split(BIndex-1, LT),
    List3 ++ [M|List4].

swap(L, I, J) ->
    {List1, [F|List2]} = lists:split(I-1, L),
    LT = List1 ++ [lists:nth(J, L)|List2],
    {List3, [_|List4]} = lists:split(J-1, LT),
    List3 ++ [F|List4].

swap_inside(N, I, M, J) ->
    {NPrev, [NVal|NPost]} = lists:split(I-1, N),
    {MPrev, [MVal|MPost]} = lists:split(J-1, M),
    {NPrev ++ [MVal|NPost], MPrev ++ [NVal|MPost]}.

mutate_give(Chromosome, I, J, K) ->
    {List1, [FL|List2]} = lists:split(I-1, Chromosome),
    {ListIn1, [F|ListIn2]} = lists:split(K-1, FL),
    LT = List1 ++ [(ListIn1++ListIn2)|List2],
    {List3, [G|List4]} = lists:split(J-1, LT),
    List3 ++ [[F|G]|List4].
