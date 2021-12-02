#!/usr/bin/env escript
-mode(compile).

move(<<"forward">>, Distance, Horizontal, Depth) ->
    {Horizontal + Distance, Depth};
move(<<"up">>, Distance, Horizontal, Depth) ->
    {Horizontal, Depth - Distance};
move(<<"down">>, Distance, Horizontal, Depth) ->
    {Horizontal, Depth + Distance}.

process_lines([Line | Lines], {Horizontal, Depth}) ->
    [Direction, DistanceStr] = binary:split(Line, <<" ">>),
    Distance = binary_to_integer(DistanceStr),

    {Horizontal1, Depth1} = move(Direction, Distance, Horizontal, Depth),
    process_lines(Lines, {Horizontal1, Depth1});
process_lines(_, {Horizontal, Depth}) ->
    {Horizontal, Depth}.

part_one(Lines) ->
    {Horizontal, Depth} = process_lines(Lines, {0, 0}),
    io:format("~p~n", [Horizontal * Depth]).

aimed_move(<<"forward">>, Distance, Horizontal, Depth, Aim) ->
    {Horizontal + Distance, Depth + (Aim * Distance), Aim};
aimed_move(<<"up">>, Distance, Horizontal, Depth, Aim) ->
    {Horizontal, Depth, Aim - Distance};
aimed_move(<<"down">>, Distance, Horizontal, Depth, Aim) ->
    {Horizontal, Depth, Aim + Distance}.

process_lines_with_aim([Line | Lines], {Horizontal, Depth, Aim}) ->
    [Direction, DistanceStr] = binary:split(Line, <<" ">>),
    Distance = binary_to_integer(DistanceStr),

    {Horizontal1, Depth1, Aim1} = aimed_move(Direction, Distance, Horizontal, Depth, Aim),
    process_lines_with_aim(Lines, {Horizontal1, Depth1, Aim1});
process_lines_with_aim(_, {Horizontal, Depth, Aim}) ->
    {Horizontal, Depth, Aim}.

part_two(Lines) ->
    {Horizontal, Depth, _Aim} = process_lines_with_aim(Lines, {0, 0, 0}),
    io:format("~p~n", [Horizontal * Depth]).

main([InputFilename]) ->
    {ok, BinaryContents} = file:read_file(InputFilename),
    Lines = binary:split(BinaryContents, <<"\n">>, [global, trim]),

    part_one(Lines),
    part_two(Lines).
