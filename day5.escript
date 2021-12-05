#!/usr/bin/env escript
-mode(compile).

parse_line_segments([Line | Lines], LineSegments) ->
    [Start, End] = binary:split(Line, <<" -> ">>, [global]),
    [X1, Y1] = binary:split(Start, <<",">>),
    [X2, Y2] = binary:split(End, <<",">>),
    parse_line_segments(Lines, [{{binary_to_integer(X1), binary_to_integer(Y1)}, {binary_to_integer(X2), binary_to_integer(Y2)}} | LineSegments]);
parse_line_segments(_, LineSegments) ->
    LineSegments.

sort_segment({{X1, Y1}, {X2, Y2}}) when X1 > X2, Y1 > Y2 -> {{X2, Y2}, {X1, Y1}};
sort_segment({{X1, Y1}, {X2, Y2}}) when X1 < X2, Y1 < Y2 -> {{X1, Y1}, {X2, Y2}};
sort_segment({{X1, Y1}, {X2, Y2}}) when X1 > X2, Y1 < Y2 -> {{X2, Y2}, {X1, Y1}};
sort_segment({{X1, Y1}, {X2, Y2}}) when X1 < X2, Y1 > Y2 -> {{X1, Y1}, {X2, Y2}};
sort_segment({{X1, Y1}, {X2, Y2}}) when X1 == X2, Y1 >= Y2 -> {{X2, Y2}, {X1, Y1}};
sort_segment({{X1, Y1}, {X2, Y2}}) when X1 == X2, Y1 < Y2 -> {{X1, Y1}, {X2, Y2}};
sort_segment({{X1, Y1}, {X2, Y2}}) when Y1 == Y2, X1 >= X2 -> {{X2, Y2}, {X1, Y1}};
sort_segment({{X1, Y1}, {X2, Y2}}) when Y1 == Y2, X1 < X2 -> {{X1, Y1}, {X2, Y2}}.

draw_line({{X1, Y1}, {X2, Y2}}, CountMap) when X1 == X2, Y1 == Y2 ->
    Count = maps:get({X1, Y1}, CountMap, 0),
    CountMap#{{X1, Y1} => Count + 1};
draw_line({{X1, Y1}, {X2, Y2}}, CountMap) when X1 < X2, Y1 < Y2 ->
    Count = maps:get({X1, Y1}, CountMap, 0),
    draw_line({{X1 + 1, Y1 + 1}, {X2, Y2}}, CountMap#{{X1, Y1} => Count + 1});
draw_line({{X1, Y1}, {X2, Y2}}, CountMap) when X1 < X2, Y1 > Y2 ->
    Count = maps:get({X1, Y1}, CountMap, 0),
    draw_line({{X1 + 1, Y1 - 1}, {X2, Y2}}, CountMap#{{X1, Y1} => Count + 1});
draw_line({{X1, Y1}, {X2, Y2}}, CountMap) when X1 == X2 ->
    Count = maps:get({X1, Y1}, CountMap, 0),
    draw_line({{X1, Y1 + 1}, {X2, Y2}}, CountMap#{{X1, Y1} => Count + 1});
draw_line({{X1, Y1}, {X2, Y2}}, CountMap) when Y1 == Y2 ->
    Count = maps:get({X1, Y1}, CountMap, 0),
    draw_line({{X1 + 1, Y1}, {X2, Y2}}, CountMap#{{X1, Y1} => Count + 1}).

populate_count_map([LineSegment | LineSegments], CountMap) ->
    CountMap1 = draw_line(LineSegment, CountMap),
    populate_count_map(LineSegments, CountMap1);
populate_count_map(_, CountMap) ->
    CountMap.

part_one(Lines) ->
    LineSegments = parse_line_segments(Lines, []),
    FilterFun = fun(Elem) ->
                        case Elem of
                            {{X1, Y1}, {X2, Y2}} when X1 == X2; Y1 == Y2 ->
                                true;
                            _ ->
                                false
                        end
                end,
    LineSegments1 = lists:filter(FilterFun, LineSegments),

    SortFun = fun(Elem, Acc) ->
                      [sort_segment(Elem) | Acc]
              end,
    SortedLineSegments = lists:foldl(SortFun, [], LineSegments1),

    CountMap = populate_count_map(SortedLineSegments, #{}),
    CountFun = fun (_, Count, Acc) ->
                       case Count >= 2 of
                           true -> Acc +1;
                           _ -> Acc
                       end
               end,
    Count = maps:fold(CountFun, 0, CountMap),
    io:format("~p~n", [Count]).

part_two(Lines) ->
    LineSegments = parse_line_segments(Lines, []),
    SortFun = fun(Elem, Acc) ->
                      [sort_segment(Elem) | Acc]
              end,
    SortedLineSegments = lists:foldl(SortFun, [], LineSegments),
    CountMap = populate_count_map(SortedLineSegments, #{}),
    CountMap = populate_count_map(SortedLineSegments, #{}),
    CountFun = fun (_, Count, Acc) ->
                       case Count >= 2 of
                           true -> Acc +1;
                           _ -> Acc
                       end
               end,
    Count = maps:fold(CountFun, 0, CountMap),
    io:format("~p~n", [Count]).

main([InputFilename]) ->
    {ok, BinaryContents} = file:read_file(InputFilename),
    Lines = binary:split(BinaryContents, <<"\n">>, [global, trim]),

    part_one(Lines),
    part_two(Lines).
