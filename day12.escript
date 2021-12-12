#!/usr/bin/env escript
-mode(compile).

is_small_cave(Cave) ->
    lists:all(fun(Elem) -> (Elem >= $a) and (Elem =< $z) end, Cave).

build_adjacency_map(Lines) ->
    lists:foldl(fun(Line, AdjacencyMap) ->
                        [A, B] = string:split(Line, "-"),
                        AdjacencyListA = maps:get(A, AdjacencyMap, []),
                        AdjacencyListB = maps:get(B, AdjacencyMap, []),
                        AdjacencyMap#{A => [B | AdjacencyListA], B => [A | AdjacencyListB]}
                end, #{}, Lines).

find_all_paths(Node, Path, Paths, AdjacencyMap) ->
    case Node of
        "end" -> [[Node | Path] | Paths];
        _ ->
            NodeInPath = lists:member(Node, Path),
            IsSmallCave = is_small_cave(Node),
            case NodeInPath and IsSmallCave of
                true -> Paths;
                _ ->
                    #{ Node := AdjacencyList} = AdjacencyMap,
                    lists:foldl(fun(Node1, Paths1) ->
                                        find_all_paths(Node1, [Node | Path], Paths1, AdjacencyMap)
                                end, Paths, AdjacencyList)
            end
    end.

part_one(Lines) ->
    AdjacencyMap = build_adjacency_map(Lines),
    AllPaths = find_all_paths("start", [], [], AdjacencyMap),
    io:format("~p ~p~n", [AllPaths, length(AllPaths)]).

has_visited_small_cave_twice(Path) ->
    VisitCount = lists:foldl(fun(Node, NodeToCount) ->
                                     Count = maps:get(Node, NodeToCount, 0),
                                     NodeToCount#{Node => Count + 1}
                             end, #{}, Path),
    not lists:all(fun({Node, Count}) ->
                         IsSmallCave = is_small_cave(Node),
                         case IsSmallCave of
                             true -> Count < 2;
                             false -> true
                         end
              end, maps:to_list(VisitCount)).

find_all_paths_visit_small_twice(Node, Path, Paths, AdjacencyMap) ->
    case Node of
        "end" -> [[Node | Path]| Paths];
        _ ->
            NodeInPath = lists:member(Node, Path),
            IsSmallCave = is_small_cave(Node),
            HasVisitedSmallCaveTwice = has_visited_small_cave_twice(Path),
            case (NodeInPath and (Node == "start")) or (NodeInPath and IsSmallCave and HasVisitedSmallCaveTwice) of
                true -> Paths;
                _ ->
                    #{Node := AdjacencyList} = AdjacencyMap,
                    lists:foldl(fun(Node1, Paths1) ->
                                        find_all_paths_visit_small_twice(Node1, [Node | Path], Paths1, AdjacencyMap)
                                end, Paths, AdjacencyList)
            end
    end.

part_two(Lines) ->
    AdjacencyMap = build_adjacency_map(Lines),
    AllPaths = find_all_paths_visit_small_twice("start", [], [], AdjacencyMap),
    io:format("~p ~p~n", [AllPaths, length(AllPaths)]).

main([InputFilename]) ->
    {ok, BinaryContents} = file:read_file(InputFilename),
    Lines = lists:map(fun unicode:characters_to_list/1, binary:split(BinaryContents, <<"\n">>, [global, trim])),

    part_one(Lines),
    part_two(Lines).
