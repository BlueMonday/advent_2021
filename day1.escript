#!/usr/bin/env escript
-mode(compile).

count_increase([_], A) ->
    A;
count_increase([First, Second | Rest], A) when Second > First ->
    count_increase([Second | Rest], A + 1);
count_increase([_First, Second | Rest], A) ->
    count_increase([Second | Rest], A).

part_one(IntegerList) ->
    Increases = count_increase(IntegerList, 0),
    io:format("~p~n", [Increases]).

count_three_increase([_], A) ->
    A;
count_three_increase([First, Second, Third, Fourth | Rest], A) when Second + Third + Fourth > First + Second + Third ->
    count_three_increase([Second, Third, Fourth | Rest], A + 1);
count_three_increase([_First, Second | Rest], A) ->
    count_three_increase([Second | Rest], A).

part_two(IntegerList) ->
    Increases = count_three_increase(IntegerList, 0),
    io:format("~p~n", [Increases]).

main([InputFilename]) ->
    {ok, BinaryContents} = file:read_file(InputFilename),
    Lines = binary:split(BinaryContents, <<"\n">>, [global, trim]),
    IntegerList = lists:map(fun(X) -> list_to_integer(binary_to_list(X)) end, Lines),

    part_one(IntegerList),
    part_two(IntegerList).
