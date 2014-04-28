-module(vrp).
%% -export([main/1, main/3, individual/4]).
-compile(export_all).

-record(chromosome, {repr,
                     fit,
                     isFitActual}).

-record(vrpProblem, {nodes,
                     distancemap,
                     depot,
                     capacity,
                     overcapcoef}).

main([FilenameArg, CarNumArg, InitPopArg, OverCapCoefArg]) ->
    Filename = atom_to_list(FilenameArg),
    Cars = list_to_integer(atom_to_list(CarNumArg)),
    InitPop = list_to_integer(atom_to_list(InitPopArg)),
    OverCapCoef = list_to_integer(atom_to_list(OverCapCoefArg)),
    main(Filename, Cars, InitPop, OverCapCoef),
    init:stop();
main(_) ->
    io:format("ERROR: You must run this program with four arguments,~n"),
    io:format("- first is the name of the file with VCRP task,~n"),
    io:format("- second is the number of cars to use,~n"),
    io:format("- third is the number of individuals in init population,~n"),
    io:format("- fourth is coefficient of over-capacity penalty.~n"),
    init:stop().

main(Filename, Cars, InitPop, OverCapCoef) ->
    {A, B, C} = erlang:now(),
    random:seed(A, B, C),

    {ok, Bin} = file:read_file(Filename),
    Parsed = parse_bin(Bin),
    print_info(Parsed),
    {Capacity, Nodes, Depot} = get_content(Parsed),
    DistanceMap = compute_distance_map(Nodes),
    VRP = #vrpProblem{nodes=Nodes, distancemap=DistanceMap,
                      depot=Depot, capacity=Capacity, overcapcoef=OverCapCoef},
    InitPopulation = create_init_population(InitPop, Cars, lists:delete(Depot, proplists:get_keys(Nodes))),

    register(main, self()),

    Processes = [spawn(?MODULE, individual,
                       [{none, none, none},
                        #chromosome{repr=I, isFitActual=false, fit=0},
                        VRP,
                        {none, none}])
                 || I <- InitPopulation],
    create_tree(Processes),

    io:format("initial population created~n"),

    [H|_] = Processes,
    H ! {selection, self()},
    receive
        {selected, _, Sorted} ->
            true
    end,

    io:format("Avg fitness: ~p~n", [average_fitness(Sorted)]),
    io:format("Best: ~p~n", [hd(Sorted)]),

    PopLength = length(Sorted),
    SelectorLength = round(PopLength/3),

    Selectors = [spawn(?MODULE, tournament_selector, [Sorted, self(), 10, PopLength])
                 || _ <- lists:seq(1, SelectorLength)], % TODO: promenna misto "20"

    Children = receive_children(SelectorLength),

    replace_worst(Sorted, Children),

    H ! {selection, self()},
    receive
        {selected, _, Sorted2} ->
            true
    end,

    io:format("Avg fitness: ~p~n", [average_fitness(Sorted2)]),
    io:format("Best: ~p~n", [hd(Sorted2)]),

    [Pid ! die || Pid <- Processes],
    unregister(main),
    %% {BestSolution, VRP}.
    ok.

receive_children(Count) ->
    receive_children(Count, 0, []).

receive_children(Total, Total, Acc) ->
    Acc;
receive_children(Total, Count, Acc) ->
    receive
        {children, X, Y} ->
            receive_children(Total, Count+1, [X, Y | Acc])
    end.

replace_worst([], []) ->
    ok;
replace_worst(Values = [_|T], Children) when length(Values) > length(Children) ->
    replace_worst(T, Children);
replace_worst([{_, Pid}|TV], [HC|TC]) ->
    Pid ! {reprChange, HC},
    replace_worst(TV, TC).

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
    Cost+CapCoef*OverCapacity; % TODO: koeficient prekroceni kapacity
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
    {round(Cost), CapUsed+Cap}; % TODO: asi neni potreba byt float() presny
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
    %% TODO: jak moc v pocatecni populaci priradit jednomu autu?
    Segments = [random:uniform(round(2*length(Nodes)/CarCount)) || _ <- lists:seq(1, CarCount)],
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

individual(Pids, C = #chromosome{repr=X, isFitActual=false}, P, _) ->
    Fit = fitness(X, P),
    individual(Pids, C#chromosome{isFitActual=true, fit=Fit}, P, {none, none});
individual(Pids = {Left, Right, Root}, C = #chromosome{fit=Fit}, P, S = {RightSelected, LeftSelected}) ->
    receive
        {left, NewLeft} ->
            individual({NewLeft, Right, Root}, C, P, S);
        {right, NewRight} ->
            individual({Left, NewRight, Root}, C, P, S);
        {selection, NewRoot} ->
            case {Left, Right} of
                {none, none} ->
                    NewRoot ! {selected, self(), [{Fit, self()}]},
                    individual({Left, Right, NewRoot}, C, P, S);
                {none, RightValue} ->
                    RightValue ! {selection, self()},
                    individual({Left, Right, NewRoot}, C, P, S);
                {LeftValue, none} ->
                    LeftValue ! {selection, self()},
                    individual({Left, Right, NewRoot}, C, P, S);
                {LeftValue, RightValue} ->
                    RightValue ! {selection, self()},
                    LeftValue ! {selection, self()},
                    individual({Left, Right, NewRoot}, C, P, S)
            end;
        {selected, Left, SortedList} ->
            if
                Right == none ->
                    Sorted = lists:keymerge(1, SortedList, [{Fit, self()}]),
                    Root ! {selected, self(), Sorted},
                    individual(Pids, C, P, {none, none});
                RightSelected /= none ->
                    First = lists:keymerge(1, RightSelected, [{Fit, self()}]),
                    Sorted = lists:keymerge(1, SortedList, First),
                    Root ! {selected, self(), Sorted},
                    individual(Pids, C, P, {none, none});
                true ->
                    individual(Pids, C, P, {RightSelected, SortedList})
            end;
        {selected, Right, SortedList} ->
            if
                Left == none ->
                    Sorted = lists:keymerge(1, SortedList, [{Fit, self()}]),
                    Root ! {selected, self(), Sorted},
                    individual(Pids, C, P, {none, none});
                LeftSelected /= none ->
                    First = lists:keymerge(1, LeftSelected, [{Fit, self()}]),
                    Sorted = lists:keymerge(1, SortedList, First),
                    Root ! {selected, self(), Sorted},
                    individual(Pids, C, P, {none, none});
                true ->
                    individual(Pids, C, P, {SortedList, LeftSelected})
            end;
        {repr, Pid} ->
            Pid ! C#chromosome.repr,
            individual(Pids, C, P, S);
        {reprChange, NewRepr} ->
            individual(Pids, C#chromosome{repr=NewRepr, isFitActual=false}, P, S);
        die ->
            true
    end.

tournament_selector(Individuals, Master, TournamentCount, Length) ->
    {A, B, C} = erlang:now(),
    random:seed(A, B, C),
    tournament_selector(Individuals, Master, TournamentCount, Length, none, none).

tournament_selector(_, _, 0, _, none, none) ->
    erlang:error(bad_tournament_count);
tournament_selector(Individuals, Master, 0, _, I, J) ->
    {_, PidA} = lists:nth(I, Individuals), % TODO: bad, bad nth
    {_, PidB} = lists:nth(J, Individuals), % TODO: bad, bad nth
    PidA ! {repr, self()},
    PidB ! {repr, self()},
    crosser(Master, none, none);
tournament_selector(Individuals, Master, C, Length, I, J) ->
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
    tournament_selector(Individuals, Master, C-1, Length, MinI, MinJ).

crosser(Master, none, none) ->
    receive
        Repr ->
            crosser(Master, Repr, none)
    end;
crosser(Master, RepA, none) ->
    receive
        Repr ->
            crosser(Master, RepA, Repr)
    end;
crosser(Master, RepA, RepB) ->
    {ChildA, ChildB} = crossover(RepA, RepB),
    Master ! {children, ChildA, ChildB}.

create_tree([]) ->
    erlang:error(no_processes);
create_tree(Processes=[_|Rest]) ->
    create_tree(Processes, Rest).

create_tree([], _) ->
    ok;
create_tree(_, []) ->
    ok;
create_tree([H|_], [L]) ->
    H ! {left, L},
    ok;
create_tree([H | T], [L, R | Rest]) ->
    H ! {left, L},
    H ! {right, R},
    create_tree(T, Rest).

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
