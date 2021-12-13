#!/usr/bin/env escript
-mode(compile).

parse_points(PointsSection)->
    PointsList = binary:split(PointsSection, <<"\n">>, [global]),
    lists:map(fun(Elem) ->
                      [X, Y] = binary:split(Elem, <<",">>),
                      {binary_to_integer(X), binary_to_integer(Y)}
              end, PointsList).

fold(PointsSet, Axis, Position) ->
    case Axis of
        <<"y">> ->
            ordsets:fold(fun({X, Y}, PointsSet1) ->
                                 case Y > Position of
                                     true -> ordsets:add_element({X, Position - (Y - Position)}, PointsSet1);
                                     false -> ordsets:add_element({X, Y}, PointsSet1)
                                 end
                         end, ordsets:new(), PointsSet);
        <<"x">> ->
            ordsets:fold(fun({X, Y}, PointsSet1) ->
                                 case X > Position of
                                     true -> ordsets:add_element({Position - (X - Position), Y}, PointsSet1);
                                     false -> ordsets:add_element({X, Y}, PointsSet1)
                                 end
                         end, ordsets:new(), PointsSet)
    end.

part_one(Parts) ->
    [PointsSection, FoldsSection] = Parts,
    PointsSet = ordsets:from_list(parse_points(PointsSection)),

    FirstFold = lists:nth(1, binary:split(FoldsSection, <<"\n">>, [global, trim])),
    [_, _, FoldInfo] = binary:split(FirstFold, <<" ">>, [global]),
    [Axis, Position] = binary:split(FoldInfo, <<"=">>),

    PointsSet1 = fold(PointsSet, Axis, binary_to_integer(Position)),

    io:format("~p~n", [length(PointsSet1)]).

print_points(PointsSet) ->
    {MaxX, MaxY} = lists:foldl(fun({X, Y}, {MaxX, MaxY}) ->
                                       {max(X, MaxX), max(Y, MaxY)}
                               end, {0, 0}, PointsSet),
    lists:foreach(fun(Y) ->
                          lists:foreach(fun(X) ->
                                                Char = case ordsets:is_element({X, Y}, PointsSet) of
                                                           true -> "#";
                                                           false -> " "
                                                       end,
                                                io:format("~s", [Char])
                                        end, lists:seq(0, MaxX)),
                          io:format("~n", [])
                  end, lists:seq(0, MaxY)).

part_two(Parts) ->
    [PointsSection, FoldsSection] = Parts,
    PointsSet = ordsets:from_list(parse_points(PointsSection)),

    Folds = binary:split(FoldsSection, <<"\n">>, [global, trim]),
    PointsSet2 = lists:foldl(fun(Fold, PointsSet1) ->
                                     [_, _, FoldInfo] = binary:split(Fold, <<" ">>, [global]),
                                     [Axis, Position] = binary:split(FoldInfo, <<"=">>),

                                     fold(PointsSet1, Axis, binary_to_integer(Position))
                             end, PointsSet, Folds),
    print_points(PointsSet2).

main([InputFilename]) ->
    {ok, BinaryContents} = file:read_file(InputFilename),
    Parts = binary:split(BinaryContents, <<"\n\n">>, [global, trim]),

    part_one(Parts),
    part_two(Parts).
