%%
%% vrp.erl
%%
%% Optimalizace CVRP pomoci genetickych algoritmu
%% Jan Sedlak, xsedla85
%%

-module(vrp).
-export([main/0, main/1, main/7, individual/4, tournament_selector/4,
         generations_iterate/4]).

%% reprezentace chromozomu pro proces
-record(chromosome, {repr, % samotna reprezentace
                     fit, % vysledek fitness
                     isFitActual}). % je vysledek fitness aktualni?

%% reprezentace VRP
-record(vrpProblem, {nodes, % seznam mest
                     distancemap, % mapa vzdalenosti
                     depot, % pozice centraly
                     capacity, % kapacita jednoho auta
                     overcapcoef, % koeficient prekroceni kapacity
                     popcount, % velikost populace
                     mutateprob, % pravdepodobnost mutace
                     tournament}). % pocet hodu pro turnaj

%% vstupni bod programu pri spousteni z konzole
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

%% vypis napovedy
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

%% vstupni bod programu
main(Filename, Cars, PopCount, OverCapCoef, Iterations, MutateProb, Tournament) ->
    random:seed(erlang:now()),

    {ok, Bin} = file:read_file(Filename), % nacte vstupni soubor
    Parsed = parse_bin(Bin), % ziska informace o uloze
    print_info(Parsed),
    {Capacity, Nodes, Depot} = get_content(Parsed), % rozparsuje zadani ulohy
    DistanceMap = compute_distance_map(Nodes),
    VRP = #vrpProblem{nodes=Nodes, distancemap=DistanceMap,
                      depot=Depot, capacity=Capacity, overcapcoef=OverCapCoef,
                      popcount=PopCount, mutateprob=MutateProb,
                      tournament=Tournament},
    InitPopulation = create_init_population(PopCount, Cars, lists:delete(Depot, proplists:get_keys(Nodes))),

    register(main, self()),

    %% vytvori procesy pro reprezentaci jedincu v populaci
    Processes = [spawn(?MODULE, individual,
                       [{none, none, none},
                        #chromosome{repr=I, isFitActual=false, fit=0},
                        VRP,
                        {none, none}])
                 || I <- InitPopulation],
    create_tree(Processes), % vytvori strom z procesu

    io:format("initial population created~n"),

    [H|_] = Processes, % prvni proces, pres ktery jde komunikace

    %% vytvori proces pro praci s generaci
    spawn(?MODULE, generations_iterate, [self(), H, Iterations, VRP]),

    receive
        {soln, {BestFit, BestPid}} -> % ziskani nejlepsiho reseni VRP
            BestPid ! {repr, self()},
            receive
                BestSolution ->
                    io:format("Best: ~p, fitness: ~p~n", [BestSolution, BestFit])
            end;
        die ->
            BestSolution = none
    end,

    [Pid ! die || Pid <- Processes], % zastaveni procesu ulohy
    unregister(main),
    {BestSolution, VRP}.

generations_iterate(Master, H, Count, VRP) ->
    H ! {selection, self()},
    receive
        {selected, _, Sorted} ->
            true
    end,
    generations_iterate(Master, H, Count, VRP, 0, Sorted).

%% iteruje postupne nad vsemi generacemi
generations_iterate(Master, _, Total, _, Total, Acc) ->
    Master ! {soln, hd(Acc)}; % na konci zasle mainu reseni
generations_iterate(Master, H, Total, VRP = #vrpProblem{popcount=PopCount,
                                                        mutateprob=MutateProb,
                                                        tournament=Tournament},
                    Count, Acc) ->
    SelectorLength = round(PopCount/3),

    %% vytvori procesy pro selekci a krizeni jedincu
    [spawn(?MODULE, tournament_selector, [Acc, self(), Tournament, PopCount])
     || _ <- lists:seq(1, SelectorLength)],

    %% ziska odpovedi od selektoru
    Children = receive_children(SelectorLength, MutateProb),

    %% nahradi nejhorsi dve tretiny populace potomky
    replace_worst(Acc, Children),

    %% ziska open serazeny seznam podle fitness
    H ! {selection, self()},
    receive
        {selected, _, NewValues} ->
            true
    end,

    {BestFit, _} = hd(NewValues),
    io:format("~p. generation: Avg fitness: ~p, Best: ~p~n", [Count, round(average_fitness(NewValues)), BestFit]),
    generations_iterate(Master, H, Total, VRP, Count+1, NewValues).

%% ziska potomky od procesu, co provadi selekci a krizeni
receive_children(Count, MutateProb) ->
    receive_children(Count, MutateProb, 0, []).

receive_children(Total, _, Total, Acc) ->
    Acc;
receive_children(Total, MutateProb, Count, Acc) ->
    receive
        {children, X, Y} ->
            case random:uniform(100) =< MutateProb of %% jeste s danou pravdepodobnosti mutuji
                true ->
                    receive_children(Total, MutateProb,  Count+1, [mutate(X), mutate(Y) | Acc]);
                false ->
                    receive_children(Total, MutateProb, Count+1, [X, Y | Acc])
            end
    end.

%% nahradi nejhorsi dve tretiny potomky z krizeni
replace_worst([], []) ->
    ok;
replace_worst(Values = [_|T], Children) when length(Values) > length(Children) ->
    replace_worst(T, Children);
replace_worst([{_, Pid}|TV], [HC|TC]) ->
    Pid ! {reprChange, HC}, %% zasle zadost o zmenu reprezentace
    replace_worst(TV, TC).

%% prevede binarni vstup na list radku
parse_bin(Bin) ->
    [string:strip(X) || X <- string:tokens(binary_to_list(Bin), "\r\n")].

%% vypise informace o uloze
print_info(Parsed) ->
    lists:map(fun(X) -> io:format("~s~n", [X]) end, lists:takewhile(fun(X) -> X /= "NODE_COORD_SECTION" end, Parsed)),
    io:format("...~n").

%% proparsuje vstup, ziska zadani ulohy
get_content(Parsed) ->
    %% preskoci az k CAPACITY
    [CapacityWhole|_] = lists:dropwhile(fun(X) -> string:substr(X, 1, length("CAPACITY")) /= "CAPACITY" end, Parsed),
    %% ziska kapacitu aut
    [_|CapacityStr] = lists:dropwhile(fun(X) -> X /= $: end, CapacityWhole),
    Capacity = list_to_integer(string:strip(CapacityStr)),

    %% preskoci az k sekci NODE_COORD_SECTION
    [_|Data] = lists:dropwhile(fun(X) -> X /= "NODE_COORD_SECTION" end, Parsed),
    {NodesStr, [_|Rest]} = lists:splitwith(fun(X) -> X /= "DEMAND_SECTION" end, Data),
    {DemandsStr, [_,DepotStr|_]} = lists:splitwith(fun(X) -> X /= "DEPOT_SECTION" end, Rest),

    %% proparsuje pozadavky
    DemandsMap = [list_to_tuple(lists:map(fun(X) -> {I, _} = string:to_integer(X), I end, string:tokens(Node, " ")))
                  || Node <- DemandsStr],

    %% proparsuje mesta
    NodesList = [lists:map(fun(X) -> {I, _} = string:to_integer(X), I end, string:tokens(Node, " "))
                 || Node <- NodesStr],
    Nodes = lists:map(fun(X) -> {I, A, B} = list_to_tuple(X), {_, Demand} = lists:keyfind(I, 1, DemandsMap),
                                {I, {A, B, Demand}} end, NodesList),

    {Depot, _} = string:to_integer(DepotStr),

    {Capacity, Nodes, Depot}.

%% spocita eukleidovskou vzdalenost vsech dvojic mest
compute_distance_map(Nodes) ->
    [{{A, B}, math:sqrt((X1-X2)*(X1-X2) + (Y1-Y2)*(Y1-Y2))} || {A, {X1, Y1, _}} <- Nodes, {B, {X2, Y2, _}} <- Nodes].

%% spocita fitness funkci zadaneho chromozomu
fitness(Chromosome, #vrpProblem{nodes=Nodes, distancemap=DistanceMap,
                                depot=Depot, capacity=Capacity, overcapcoef=CapCoef})
  when length(Chromosome) > 0 ->
    fitness(Chromosome, Nodes, DistanceMap, Depot, Capacity, CapCoef, 0, 0);
fitness(_, _) ->
    erlang:error(bad_chromosome).

fitness([], _, _, _, _, CapCoef, Cost, OverCapacity) when OverCapacity > 0 ->
    Cost+CapCoef*OverCapacity; % prekrocil kapacitu
fitness([], _, _, _, _, _, Cost, _) ->
    Cost;
fitness([H|T], Nodes, DistanceMap, Depot, Capacity, CapCoef, Cost, OverCapacity) ->
    {ActualCost, ActualCap} = fitness_dist([Depot|H]++[Depot], Nodes, DistanceMap),
    ActualOverCap = if ActualCap > Capacity -> (ActualCap - Capacity); true -> 0 end,
    %% pocita hodnotu dalsiho vozidla
    fitness(T, Nodes, DistanceMap, Depot, Capacity, CapCoef, Cost+ActualCost, OverCapacity+ActualOverCap).

%% spocita soucet vzdalenosti seznamu bodu
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

%% spocita prumernou hodnotu fitness populace
average_fitness(Values) ->
    lists:foldl(fun({V, _}, Acc) -> Acc + V end, 0, Values)/length(Values).

%% vytvori permutaci seznamu
shuffle(List) ->
    [X || {_, X} <- lists:sort([{random:uniform(), N} || N <- List])].

%% vytvori pocatecni populaci se zadanou velikosti
create_init_population(0, _, _) ->
    [];
create_init_population(Count, CarCount, Nodes) ->
    create_init_population(Count, CarCount, Nodes, []).

create_init_population(0, _, _, List) ->
    List;
create_init_population(Count, CarCount, Nodes, List) ->
    Shuffled = shuffle(Nodes), % vytvori permutaci
    Segments = [random:uniform(length(Nodes)) || _ <- lists:seq(1, CarCount)], % pripravy segmenty
    Cutted = cut_list(Shuffled, Segments), % rozkouskuje seznam podle segmentu
    create_init_population(Count-1, CarCount, Nodes, [Cutted|List]).

%% rozkouskuje seznam podle segmentu
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

%% funkce procesu reprezentujici jedince
individual(Pids, C = #chromosome{repr=X, isFitActual=false}, P, _) -> % neni spocitana fitness
    Fit = fitness(X, P),
    individual(Pids, C#chromosome{isFitActual=true, fit=Fit}, P, {none, none});
individual(Pids = {Left, Right, Root}, C = #chromosome{fit=Fit}, P, S = {RightSelected, LeftSelected}) ->
    receive % ceka na prijeti zpravy
        {left, NewLeft} -> % zaslani informace o pravem potomkovi
            individual({NewLeft, Right, Root}, C, P, S);
        {right, NewRight} -> % zaslani informace o levem potomkovi
            individual({Left, NewRight, Root}, C, P, S);
        {selection, NewRoot} -> % zaslani pozadavku o serazeni
            case {Left, Right} of
                {none, none} -> % ani jedna odpoved
                    NewRoot ! {selected, self(), [{Fit, self()}]},
                    individual({Left, Right, NewRoot}, C, P, S);
                {none, RightValue} -> % pravy odpovedel
                    RightValue ! {selection, self()},
                    individual({Left, Right, NewRoot}, C, P, S);
                {LeftValue, none} -> % levy odpovedel
                    LeftValue ! {selection, self()},
                    individual({Left, Right, NewRoot}, C, P, S);
                {LeftValue, RightValue} -> % oba odpovedeli
                    RightValue ! {selection, self()},
                    LeftValue ! {selection, self()},
                    individual({Left, Right, NewRoot}, C, P, S)
            end;
        {selected, Left, SortedList} -> % ziskani serazene posloupnosti z leveho podstromu
            if
                Right == none -> % nema pravy podstrom
                    Sorted = lists:keymerge(1, SortedList, [{Fit, self()}]),
                    Root ! {selected, self(), Sorted},
                    individual(Pids, C, P, {none, none});
                RightSelected /= none -> % pravy uz odpovedel
                    First = lists:keymerge(1, RightSelected, [{Fit, self()}]),
                    Sorted = lists:keymerge(1, SortedList, First),
                    Root ! {selected, self(), Sorted},
                    individual(Pids, C, P, {none, none});
                true -> % cekam na odpoved praveho
                    individual(Pids, C, P, {RightSelected, SortedList})
            end;
        {selected, Right, SortedList} -> % ziskani serazene posloupnosti z praveho podstromu
            if
                Left == none -> % nema levy podstrom
                    Sorted = lists:keymerge(1, SortedList, [{Fit, self()}]),
                    Root ! {selected, self(), Sorted},
                    individual(Pids, C, P, {none, none});
                LeftSelected /= none -> % levy uz odpovedel
                    First = lists:keymerge(1, LeftSelected, [{Fit, self()}]),
                    Sorted = lists:keymerge(1, SortedList, First),
                    Root ! {selected, self(), Sorted},
                    individual(Pids, C, P, {none, none});
                true -> % cekam na odpoved leveho
                    individual(Pids, C, P, {SortedList, LeftSelected})
            end;
        {repr, Pid} -> % pozadavek na zaslani reprezentace
            Pid ! C#chromosome.repr,
            individual(Pids, C, P, S);
        {reprChange, NewRepr} -> % pozadavek na zmenu reprezentace
            individual(Pids, C#chromosome{repr=NewRepr, isFitActual=false}, P, S);
        die -> % ukonceni procesu
            true
    end.

%% funkce pro proces, ktery provadi vyber a krizeni
tournament_selector(Individuals, Master, TournamentCount, Length) ->
    random:seed(erlang:now()),
    tournament_selector(Individuals, Master, TournamentCount, Length, none, none).

%% Nkrat vybere nahodne cislo a necha to nejmensi
tournament_selector(_, _, 0, _, none, none) ->
    erlang:error(bad_tournament_count);
tournament_selector(Individuals, Master, 0, _, I, J) ->
    {_, PidA} = lists:nth(I, Individuals),
    {_, PidB} = lists:nth(J, Individuals),
    PidA ! {repr, self()},
    PidB ! {repr, self()},
    crosser(Master, none, none); % zkrizeni Iteho a Jteho jedicne
tournament_selector(Individuals, Master, C, Length, I, J) ->
    First = random:uniform(Length), % vygenerovani novych cisel
    Second = random:uniform(Length),
    MinI = if First < I -> % ponechani pouze mensich
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

%% funkce pro ziskani reprezentaci pro krizeni
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
    {ChildA, ChildB} = crossover(RepA, RepB), % zkrizeni
    Master ! {children, ChildA, ChildB}. % zaslani odpovedi zpet

%% funkce pro vytvoreni stromu procesoru
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

%% funkce pro krizeni dvou chromozomu
crossover(ChromosomeA, ChromosomeB) ->
    %% vytvori se seznam ze seznamu seznamu
    IndexTable = lists:sort(lists:flatten(ChromosomeA)),
    %% vytvori se indexove chromozomy
    IndexChromA = create_ichromosome(ChromosomeA, IndexTable),
    IndexChromB = create_ichromosome(ChromosomeB, IndexTable),
    %% nahodne se prohodi jejich mesta
    SplitIndex = random:uniform(length(IndexChromA)),
    {FirstA, SecondA} = lists:split(SplitIndex, IndexChromA),
    {FirstB, SecondB} = lists:split(SplitIndex, IndexChromB),
    %% prevedou ze z indexoveho chromozomu zpet
    FirstIndexChrom = from_ichrom(FirstA ++ SecondB, IndexTable),
    SecondIndexChrom = from_ichrom(FirstB ++ SecondA, IndexTable),
    %% prvni se prevede zpet na seznam seznamu podle A, druhy podle B
    ChildA = unflatten_by(FirstIndexChrom, ChromosomeA),
    ChildB = unflatten_by(SecondIndexChrom, ChromosomeB),
    {ChildA, ChildB}.

%% vytvori indexovy chromozom
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

%% vytvori chromozom zpet z indexoveho chromozomu
from_ichrom(IndexChromosome, IndexTable) ->
    from_ichrom(IndexChromosome, IndexTable, []).

from_ichrom([], _, Acc) ->
    lists:reverse(Acc);
from_ichrom([H|T], IndexTable, Acc) ->
    Value = lists:nth(H, IndexTable),
    IndexTableChanged = lists:delete(Value, IndexTable),
    from_ichrom(T, IndexTableChanged, [Value|Acc]).

% vrati index prvku v seznamu
index_of(Item, List) ->
    index_of(Item, List, 1).

index_of(_, [], _) ->
    erlang:error(cannot_find_value);
index_of(Item, [Item|_], I) ->
    I;
index_of(Item, [_|T], I) ->
    index_of(Item, T, I+1).

%% prevede seznam na seznam seznamu podle zadaneho vzoru
unflatten_by(FlatList, Chromosome) ->
    unflatten_by(FlatList, Chromosome, []).

unflatten_by([], _, Acc) ->
    lists:reverse(Acc);
unflatten_by(FlatList, [H|T], Acc) ->
    {First, Second} = lists:split(length(H), FlatList),
    unflatten_by(Second, T, [First|Acc]).

%% funkce pro mutovani chromozomu
mutate(Chromosome) ->
    I = random:uniform(length(Chromosome)),
    J = random:uniform(length(Chromosome)),
    case random:uniform(2) of
        1 -> % prehazuju mesta
            if I == J -> % v ramci stejne cesty
                    Splited = lists:split(I-1, Chromosome),
                    mutate(Chromosome, Splited);
               true -> % v ramci dvou cest
                    As = lists:split(I-1, Chromosome),
                    B = lists:nth(J, Chromosome),
                    mutate(Chromosome, As, B, J)
            end;
        2 -> % odebiram a pridavam mesto
            Length = length(lists:nth(I, Chromosome)),
            if Length == 0 ->
                    mutate(Chromosome);
               true ->
                    K = random:uniform(Length),
                    mutate_give(Chromosome, I, J, K)
            end
    end.

%% prehazuje dve mesta v ramci jednoho seznamu
mutate(Chromosome, {_, [N|_]}) when length(N) == 0 ->
    mutate(Chromosome);
mutate(_, {NPrev, [N|NPost]}) ->
    I = random:uniform(length(N)),
    J = random:uniform(length(N)),
    NPrev ++ [swap(N, I, J) | NPost].

%% prehazuje dve mesta ve dvou seznamech
mutate(Chromosome, {_, [A|_]}, B, _) when length(A) == 0; length(B) == 0 ->
    mutate(Chromosome);
mutate(_, {APrev, [A|APost]}, B, BIndex) ->
    I = random:uniform(length(A)),
    J = random:uniform(length(B)),
    {N, M} = swap_inside(A, I, B, J),
    LT = APrev ++ [N|APost],
    {List3, [_|List4]} = lists:split(BIndex-1, LT),
    List3 ++ [M|List4].

%% prohodi polozky na dvou zadanych mistech v seznamu
swap(L, I, J) ->
    {List1, [F|List2]} = lists:split(I-1, L),
    LT = List1 ++ [lists:nth(J, L)|List2],
    {List3, [_|List4]} = lists:split(J-1, LT),
    List3 ++ [F|List4].

swap_inside(N, I, M, J) ->
    {NPrev, [NVal|NPost]} = lists:split(I-1, N),
    {MPrev, [MVal|MPost]} = lists:split(J-1, M),
    {NPrev ++ [MVal|NPost], MPrev ++ [NVal|MPost]}.

%% odebere mesto z jedne cesty a prida ho k jine ceste
mutate_give(Chromosome, I, J, K) ->
    {List1, [FL|List2]} = lists:split(I-1, Chromosome),
    {ListIn1, [F|ListIn2]} = lists:split(K-1, FL),
    LT = List1 ++ [(ListIn1++ListIn2)|List2],
    {List3, [G|List4]} = lists:split(J-1, LT),
    List3 ++ [[F|G]|List4].
