#!/usr/bin/env escript
-mode(compile).

build_octo_map(Lines) ->
    OctoKeyValTuples = [{{X, Y}, lists:nth(X, lists:nth(Y, Lines))} || Y <- lists:seq(1, 10), X <- lists:seq(1, 10)],
    maps:from_list(OctoKeyValTuples).

flash(Y, X, OctoMap, AlreadyFlashed, Flashes) ->
    case maps:is_key({X, Y}, OctoMap) of
        false -> {OctoMap, AlreadyFlashed, Flashes};
        true ->
            case maps:get({X, Y}, AlreadyFlashed, false) of
                true ->
                    {OctoMap, AlreadyFlashed, Flashes};
                false ->
                    Value = maps:get({X, Y}, OctoMap),
                    case Value + 1 of
                        Value1 when Value1 > 9 ->
                            AlreadyFlashed1 = AlreadyFlashed#{{X, Y} => true},
                            Flashes1 = Flashes + 1,
                            OctoMap1 = OctoMap#{{X, Y} => 0},

                            {OctoMap2, AlreadyFlashed2, Flashes2} = flash(Y - 1, X - 1, OctoMap1, AlreadyFlashed1, Flashes1),
                            {OctoMap3, AlreadyFlashed3, Flashes3} = flash(Y - 1, X, OctoMap2, AlreadyFlashed2, Flashes2),
                            {OctoMap4, AlreadyFlashed4, Flashes4} = flash(Y - 1, X + 1, OctoMap3, AlreadyFlashed3, Flashes3),
                            {OctoMap5, AlreadyFlashed5, Flashes5} = flash(Y, X - 1, OctoMap4, AlreadyFlashed4, Flashes4),
                            {OctoMap6, AlreadyFlashed6, Flashes6} = flash(Y, X + 1, OctoMap5, AlreadyFlashed5, Flashes5),
                            {OctoMap7, AlreadyFlashed7, Flashes7} = flash(Y + 1, X - 1, OctoMap6, AlreadyFlashed6, Flashes6),
                            {OctoMap8, AlreadyFlashed8, Flashes8} = flash(Y + 1, X, OctoMap7, AlreadyFlashed7, Flashes7),
                            flash(Y + 1, X + 1, OctoMap8, AlreadyFlashed8, Flashes8);
                        Value1 ->
                            OctoMap1 = OctoMap#{{X, Y} => Value1},
                            {OctoMap1, AlreadyFlashed, Flashes}
                    end
            end
    end.

flash_row(Y, OctoMap, AlreadyFlashed, Flashes) ->
    FoldFun = fun(X, {OctoMap1, AlreadyFlashed1, Flashes1}) ->
                      flash(Y, X, OctoMap1, AlreadyFlashed1, Flashes1)
              end,
    lists:foldl(FoldFun, {OctoMap, AlreadyFlashed, Flashes}, lists:seq(1, 10)).

simulate_day(OctoMap) ->
    FoldFun = fun(Y, {OctoMap1, AlreadyFlashed, Flashes}) -> flash_row(Y, OctoMap1, AlreadyFlashed, Flashes) end,
    {OctoMap1, _, Flashes} = lists:foldl(FoldFun, {OctoMap, #{}, 0}, lists:seq(1, 10)),
    {OctoMap1, Flashes}.

print_map(OctoMap) ->
    lists:foreach(fun (Y) ->
                          lists:foreach(fun (X) ->
                                                io:format("~p", [maps:get({X, Y}, OctoMap)])
                                        end, lists:seq(1, 10)),
                          io:format("~n", [])
                          end, lists:seq(1, 10)),
    io:format("~n", []).

part_one(Lines) ->
    OctoMap = build_octo_map(Lines),
    FoldFun = fun(Day, {OctoMap1, Flashes}) ->
                      {OctoMap2, Flashes1} = simulate_day(OctoMap1),
                      print_map(OctoMap2),

                      {OctoMap2, Flashes + Flashes1}
              end,
    {_, Flashes} = lists:foldl(FoldFun, {OctoMap, 0}, lists:seq(1, 100)),
    io:format("~p~n", [Flashes]).

flash_until_all_flash(Day, OctoMap) ->
    {OctoMap1, Flashes} = simulate_day(OctoMap),
    if Flashes == 100 ->
            Day;
       true ->
            flash_until_all_flash(Day + 1, OctoMap1)
    end.

part_two(Lines) ->
    OctoMap = build_octo_map(Lines),
    AllFlashDay = flash_until_all_flash(1, OctoMap),
    io:format("~p~n", [AllFlashDay]).

main([InputFilename]) ->
    {ok, BinaryContents} = file:read_file(InputFilename),
    Lines = binary:split(BinaryContents, <<"\n">>, [global, trim]),
    IntLines = [[list_to_integer([Elem]) || Elem <- binary_to_list(Line)]  || Line <- Lines],

    part_one(IntLines),
    part_two(IntLines).
