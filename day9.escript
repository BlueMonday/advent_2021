#!/usr/bin/env escript
-mode(compile).

low_point(X, Y, DeltaX, DeltaY, Lines) ->
    Line = lists:nth(Y, Lines),
    Point = lists:nth(X, Line),

    MaxY = length(Lines),
    MaxX = length(Line),
    case {X + DeltaX, Y + DeltaY} of
        {X2, Y2} when X2 == 0; X2 > MaxX; Y2 == 0; Y2 > MaxY ->
            true;
        {X2, Y2} ->
            Line2 = lists:nth(Y2, Lines),
            Point2 = lists:nth(X2, Line2),
            Point < Point2
    end.

risk_level(X, Y, Lines) ->
    LowPoint = low_point(X, Y, 1, 0, Lines) andalso
        low_point(X, Y, -1, 0, Lines) andalso
        low_point(X, Y, 0, 1, Lines) andalso
        low_point(X, Y, 0, -1, Lines),

    case LowPoint of
        true ->
            Line = lists:nth(Y, Lines),
            lists:nth(X, Line) + 1;
        false -> 0
    end.

calc_risk(Lines, Y) ->
    Line = lists:nth(Y, Lines),
    FoldFun = fun(X, Sum) -> risk_level(X, Y, Lines) + Sum end,
    lists:foldl(FoldFun, 0, lists:seq(1, length(Line))).

part_one(Lines) ->
    FoldFun = fun(Y, Sum) -> Sum + calc_risk(Lines, Y) end,
    Sum = lists:foldl(FoldFun, 0, lists:seq(1, length(Lines))),
    io:format("~p~n", [Sum]).

calc_basin_size(PrevPoint, X, Y, Lines, Visited) ->
    MaxY = length(Lines),
    MaxX = length(lists:nth(1, Lines)),
    AlreadyVisited = lists:member({X, Y}, Visited),

    if
        AlreadyVisited; X == 0; X > MaxX; Y == 0; Y > MaxY ->
            {0, Visited};
        true ->
            Line = lists:nth(Y, Lines),
            Point = lists:nth(X, Line),

            if
                Point == 9; Point < PrevPoint ->
                    {0, Visited};
                true ->
                    Visited1 = [{X, Y} | Visited],

                    {Right, Visited2} = calc_basin_size(Point, X + 1, Y, Lines, Visited1),
                    {Left, Visited3} = calc_basin_size(Point, X - 1, Y, Lines, Visited2),
                    {Up, Visited4} = calc_basin_size(Point, X, Y + 1, Lines, Visited3),
                    {Down, Visited5} = calc_basin_size(Point, X, Y - 1, Lines, Visited4),

                    {1 + Right + Left + Up + Down, Visited5}
            end
    end.

calc_basin_sizes(Lines, Y) ->
    Line = lists:nth(Y, Lines),
    FoldFun = fun(X, BasinSizes) ->
                      Point = lists:nth(X, Line),
                      {BasinSize, _} = calc_basin_size(Point - 1, X, Y, Lines, []),
                      [BasinSize | BasinSizes]
              end,
    lists:foldl(FoldFun, [], lists:seq(1, length(Line))).

part_two(Lines) ->
    FoldFun = fun(Y, BasinSizes) -> BasinSizes ++ calc_basin_sizes(Lines, Y) end,
    BasinSizes = lists:foldl(FoldFun, [], lists:seq(1, length(Lines))),
    SortedBasinSizes = lists:sort(BasinSizes),
    ThreeLargestMultiplied = lists:nth(length(SortedBasinSizes), SortedBasinSizes) *
        lists:nth(length(SortedBasinSizes) - 1, SortedBasinSizes) *
        lists:nth(length(SortedBasinSizes) - 2, SortedBasinSizes),

    io:format("~p~n", [ThreeLargestMultiplied]).

main([InputFilename]) ->
    {ok, BinaryContents} = file:read_file(InputFilename),
    Lines = binary:split(BinaryContents, <<"\n">>, [global, trim]),
    IntLines = [[list_to_integer([Elem]) || Elem <- binary_to_list(Line)]  || Line <- Lines],

    part_one(IntLines),
    part_two(IntLines).
