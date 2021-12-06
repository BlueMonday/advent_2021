#!/usr/bin/env escript
-mode(compile).

simulate_fish([Fish | Fishes], UpdatedFishes) ->
    Fish1 = Fish - 1,
    case Fish1 of
        -1 -> simulate_fish(Fishes, [6, 8 | UpdatedFishes]);
        _ -> simulate_fish(Fishes, [Fish1 | UpdatedFishes])
    end;
simulate_fish(_, UpdatedFishes) ->
    UpdatedFishes.

simulate_days(Fishes, 0) ->
    Fishes;
simulate_days(Fishes, Days) ->
    Fishes1 = simulate_fish(Fishes, []),
    simulate_days(Fishes1, Days - 1).

part_one(InitialFish) ->
    Fish = simulate_days(InitialFish, 80),
    io:format("~p~n", [length(Fish)]).

smarter_simulate_days(DayToCount, 0) ->
    DayToCount;
smarter_simulate_days(DayToCount, Days) ->
    ShiftFish = fun(Day, Count, UpdatedDayToCount) ->
                              Day1 = Day - 1,
                              case Day1 of
                                  -1 ->
                                      UpdatedDayToCount#{8 => Count};
                                  _ ->
                                      UpdatedDayToCount#{Day1 => Count}
                              end
                      end,
    DayToCount1 = maps:fold(ShiftFish, #{}, DayToCount),

    Day8Count = maps:get(8, DayToCount1, 0),
    Day6Count = maps:get(6, DayToCount1, 0),
    DayToCount2 = DayToCount1#{6 => Day6Count + Day8Count},
    smarter_simulate_days(DayToCount2, Days - 1).

part_two(InitialFish) ->
    ListToMap = fun(Fish, DayToCount) ->
                        Count = maps:get(Fish, DayToCount, 0),
                        DayToCount#{Fish => Count + 1}
                end,
    DayToCount = lists:foldl(ListToMap, #{}, InitialFish),
    DayToCount1 = smarter_simulate_days(DayToCount, 256),

    SumFun = fun(_, Val, Acc) -> Acc + Val end,
    Count = maps:fold(SumFun, 0, DayToCount1),
    io:format("~p~n", [Count]).

main([]) ->
    Input = "3,5,3,1,4,4,5,5,2,1,4,3,5,1,3,5,3,2,4,3,5,3,1,1,2,1,4,5,3,1,4,5,4,3,3,4,3,1,1,2,2,4,1,1,4,3,4,4,2,4,3,1,5,1,2,3,2,4,4,1,1,1,3,3,5,1,4,5,5,2,5,3,3,1,1,2,3,3,3,1,4,1,5,1,5,3,3,1,5,3,4,3,1,4,1,1,1,2,1,2,3,2,2,4,3,5,5,4,5,3,1,4,4,2,4,4,5,1,5,3,3,5,5,4,4,1,3,2,3,1,2,4,5,3,3,5,4,1,1,5,2,5,1,5,5,4,1,1,1,1,5,3,3,4,4,2,2,1,5,1,1,1,4,4,2,2,2,2,2,5,5,2,4,4,4,1,2,5,4,5,2,5,4,3,1,1,5,4,5,3,2,3,4,1,4,1,1,3,5,1,2,5,1,1,1,5,1,1,4,2,3,4,1,3,3,2,3,1,1,4,4,3,2,1,2,1,4,2,5,4,2,5,3,2,3,3,4,1,3,5,5,1,3,4,5,1,1,3,1,2,1,1,1,1,5,1,1,2,1,4,5,2,1,5,4,2,2,5,5,1,5,1,2,1,5,2,4,3,2,3,1,1,1,2,3,1,4,3,1,2,3,2,1,3,3,2,1,2,5,2",
    InitialFish = lists:map(fun list_to_integer/1, string:split(Input, ",", all)),

    part_one(InitialFish),
    part_two(InitialFish).
