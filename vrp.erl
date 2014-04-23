-module(vrp).
%-export([main/1, main/3, individual/4]).
-compile(export_all).

-record(chromosome, {repr,
                     fit,
                     isFitActual}).

-record(vrpProblem, {nodes,
                     distancemap,
                     depot,
                     capacity}).

main([FilenameArg, CarNumArg, InitPopArg]) ->
    Filename = atom_to_list(FilenameArg),
    Cars = list_to_integer(atom_to_list(CarNumArg)),
    InitPop = list_to_integer(atom_to_list(InitPopArg)),
    main(Filename, Cars, InitPop),
    init:stop();
main(_) ->
    io:format("ERROR: You must run this program with three arguments,~n"),
    io:format("- first is the name of the file with VCRP task,~n"),
    io:format("- second is the number of cars to use,~n"),
    io:format("- third is the number of individuals in init population .~n"),
    init:stop().

main(Filename, Cars, InitPop) ->
    {ok, Bin} = file:read_file(Filename),
    Parsed = parse_bin(Bin),
    print_info(Parsed),
    {Capacity, Nodes, Depot} = get_content(Parsed),
    DistanceMap = compute_distance_map(Nodes),
    VRP = #vrpProblem{nodes=Nodes, distancemap=DistanceMap, depot=Depot, capacity=Capacity},
    InitPopulation = create_init_population(InitPop, Cars, lists:delete(Depot, proplists:get_keys(Nodes))),

    register(main, self()),

    Processes = [spawn(?MODULE, individual,
                       [{none, none, none},
                        #chromosome{repr=I, isFitActual=false, fit=0},
                        VRP,
                        {none, none}])
                 || I <- InitPopulation],
    create_tree(Processes),
    [H|_] = Processes,
    H ! {selection, self()},
    receive
        {selected, _, SelectedBest, SelectedWorst} ->
            io:format("~p ~p~n", [SelectedBest, SelectedWorst])
    end,

    {_, BestPid} = SelectedBest,

    BestPid ! {repr, self()},

    BestSolution = receive
        Data ->
            Data
    end,

    [Pid ! die || Pid <- Processes],
    unregister(main),
    {BestSolution, VRP}.
    
parse_bin(Bin) ->
    [string:strip(X) || X <- string:tokens(binary_to_list(Bin), "\r\n")].

print_info(Parsed) ->
    lists:map(fun(X) -> io:format("~s~n", [X]) end, lists:takewhile(fun(X) -> X /= "NODE_COORD_SECTION" end, Parsed)).

get_content(Parsed) ->
    [CapacityWhole|_] = lists:dropwhile(fun(X) -> string:substr(X, 1, length("CAPACITY")) /= "CAPACITY" end, Parsed),
    [_|CapacityStr] = lists:dropwhile(fun(X) -> X /= $: end, CapacityWhole),
    Capacity = list_to_integer(string:strip(CapacityStr)),

    [_|Data] = lists:dropwhile(fun(X) -> X /= "NODE_COORD_SECTION" end, Parsed),
    {NodesStr, [_|Rest]} = lists:splitwith(fun(X) -> X /= "DEMAND_SECTION" end, Data),
    {DemandsStr, [_,DepotStr|_]} = lists:splitwith(fun(X) -> X /= "DEPOT_SECTION" end, Rest),

    DemandsMap = [list_to_tuple(lists:map(fun(X) -> {I, _} = string:to_integer(X), I end, string:tokens(Node, " "))) || Node <- DemandsStr],

    NodesList = [lists:map(fun(X) -> {I, _} = string:to_integer(X), I end, string:tokens(Node, " ")) || Node <- NodesStr],
    Nodes = lists:map(fun(X) -> {I, A, B} = list_to_tuple(X), {I, {A, B, proplists:get_value(I, DemandsMap)}} end, NodesList),
    
    {Depot, _} = string:to_integer(DepotStr),

    {Capacity, Nodes, Depot}.

compute_distance_map(Nodes) ->
    [{{A, B}, math:sqrt((X1-X2)*(X1-X2) + (Y1-Y2)*(Y1-Y2))} || {A, {X1, Y1, _}} <- Nodes, {B, {X2, Y2, _}} <- Nodes].

% chromozom vypada jako: [[], [], [], [], []]
fitness(Chromosome, #vrpProblem{nodes=Nodes, distancemap=DistanceMap,
                                 depot=Depot, capacity=Capacity})
  when length(Chromosome) > 0 ->
    fitness(Chromosome, Nodes, DistanceMap, Depot, Capacity, 0, 0);
fitness(_, _) ->
    erlang:error(bad_chromosome).

fitness([], _, _, _, _, Cost, OverCapacity) when OverCapacity > 0 ->
    Cost+1000*OverCapacity; % TODO: koeficient prekroceni kapacity
    %Cost;
fitness([], _, _, _, _, Cost, _) ->
    Cost;
fitness([H|T], Nodes, DistanceMap, Depot, Capacity, Cost, OverCapacity) ->
    {ActualCost, ActualCap} = fitness_dist([Depot|H]++[Depot], Nodes, DistanceMap),
    ActualOverCap = if ActualCap > Capacity -> (ActualCap - Capacity); true -> 0 end,
    fitness(T, Nodes, DistanceMap, Depot, Capacity, Cost+ActualCost, OverCapacity+ActualOverCap).

fitness_dist([], _, _) ->
    {0, 0};
fitness_dist([H|T], Nodes, DistanceMap) ->
    fitness_dist(H, T, Nodes, DistanceMap, 0, 0).

fitness_dist(H, [], Nodes, _, Cost, CapUsed) ->
    {_, _, Cap} = proplists:get_value(H, Nodes),
    {round(Cost), CapUsed+Cap}; % TODO: asi neni potreba byt float() presny
fitness_dist(Last, [H|T], Nodes, DistanceMap, Cost, CapUsed) ->
    {_, _, Cap} = proplists:get_value(Last, Nodes),
    Distance = proplists:get_value({Last, H}, DistanceMap),
    fitness_dist(H, T, Nodes, DistanceMap, Cost+Distance, CapUsed+Cap).

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
    % TODO: jak moc v pocatecni populaci priradit jednomu autu?
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
    %io:format("~p fitness: ~p~n", [self(), Fit]),
    receive
        {left, NewLeft} ->
            individual({NewLeft, Right, Root}, C, P, S);
        {right, NewRight} ->
            individual({Left, NewRight, Root}, C, P, S);
        {selection, NewRoot} ->
            case {Left, Right} of
                {none, none} ->
                    NewRoot ! {selected, self(), {Fit, self()}, {Fit, self()}},
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
        {selected, Left, Best, Worst} ->
            if
                Right == none ->
                    {Greatest, Lowest} = select_best_worst({Best, Worst}, {{Fit, self()}, {Fit, self()}}),
                    Root ! {selected, self(), Greatest, Lowest},
                    individual(Pids, C, P, {none, none});
                RightSelected /= none ->
                    {Greatest, Lowest} = select_best_worst(RightSelected, {Best, Worst}, {{Fit, self()}, {Fit, self()}}),
                    Root ! {selected, self(), Greatest, Lowest},
                    individual(Pids, C, P, {none, none});
                true ->
                    individual(Pids, C, P, {RightSelected, {Best, Worst}})
            end;
        {selected, Right, Best, Worst} ->
            if
                Left == none ->
                    {Greatest, Lowest} = select_best_worst({Best, Worst}, {{Fit, self()}, {Fit, self()}}),
                    Root ! {selected, self(), Greatest, Lowest},
                    individual(Pids, C, P, {none, none});
                LeftSelected /= none ->
                    {Greatest, Lowest} = select_best_worst({Best, Worst}, LeftSelected, {{Fit, self()}, {Fit, self()}}),
                    Root ! {selected, self(), Greatest, Lowest},
                    individual(Pids, C, P, {none, none});
                true ->
                    individual(Pids, C, P, {{Best, Worst}, LeftSelected})
            end;
        {repr, Pid} ->
            Pid ! C#chromosome.repr,
            individual(Pids, C, P, S);
        die ->
            true
    end.

select_best_worst({B1, W1}, {B2, W2}, {B3, W3}) ->
    [B|_] = lists:sort([B1, B2, B3]),
    [_,_,W] = lists:sort([W1, W2, W3]),
    {B, W}.

select_best_worst({B1, W1}, {B2, W2}) ->
    B = if B1 < B2 ->
                B1;
           true ->
                B2
        end,
    W = if W1 > W2 ->
                W1;
           true ->
                W2
        end,
    {B, W}.

create_tree([]) ->
    erlang:error(no_processes);
create_tree(Processes) ->
    create_tree(Processes, Processes, 1).

create_tree([], _, _) ->
    ok;
create_tree(_, All, I) when length(All) < 2*I ->
    ok;
create_tree([H|_], All, I) when length(All) == 2*I ->
    H ! {left, lists:nth(2*I, All)},
    ok;
create_tree([H|Remaining], All, I) ->
    H ! {left, lists:nth(2*I, All)},
    H ! {right, lists:nth(2*I+1, All)},
    create_tree(Remaining, All, I+1).
