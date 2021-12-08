#!/usr/bin/env escript
-mode(compile).

count_easy(OutputValues) ->
    FoldFun = fun(Elem, Acc) ->
                      case length(Elem) of
                          Length when Length == 2; Length == 4; Length == 3; Length == 7 ->
                              Acc + 1;
                          _ ->
                              Acc
                      end
              end,
    lists:foldl(FoldFun, 0, OutputValues).

part_one(Lines) ->
    FoldFun = fun(Elem, Acc) ->
                      [_, OutputValues] = string:split(Elem, " | ", all),
                      OutputValuesList = string:split(OutputValues, " ", all),
                      Acc + count_easy(OutputValuesList)
              end,
    Count = lists:foldl(FoldFun, 0, Lines),
    io:format("~p~n", [Count]).

build_signals_to_number_map(SignalPatterns) ->
    BuildLengthMapFun = fun(Pattern, LengthToSignals) ->
                                Length = length(Pattern),
                                Signals = maps:get(Length, LengthToSignals, []),
                                LengthToSignals#{Length => [ordsets:from_list(Pattern) | Signals]}
                        end,
    LengthToSignals = lists:foldl(BuildLengthMapFun , #{}, SignalPatterns),

    #{ 2 := [OneSignals]} = LengthToSignals,
    #{ 4 := [FourSignals]} = LengthToSignals,
    #{ 3 := [SevenSignals]} = LengthToSignals,
    #{ 7 := [EightSignals]} = LengthToSignals,

    #{ 5 := LengthFiveSignals} = LengthToSignals,
    #{ 6 := LengthSixSignals} = LengthToSignals,

    [ASignal] = ordsets:to_list(ordsets:subtract(SevenSignals, OneSignals)),

    SignalSetToFindNine = ordsets:add_element(ASignal, FourSignals),
    [NineSignals] = lists:filter(fun(Elem) -> ordsets:is_subset(SignalSetToFindNine, Elem) end, LengthSixSignals),
    [GSignal] = ordsets:to_list(ordsets:subtract(NineSignals, SignalSetToFindNine)),

    SignalSetToFindThree = ordsets:union(OneSignals, ordsets:from_list([ASignal, GSignal])),
    [ThreeSignals] = lists:filter(fun(Elem) -> ordsets:is_subset(SignalSetToFindThree, Elem) end, LengthFiveSignals),
    [DSignal] = ordsets:to_list(ordsets:subtract(ThreeSignals, SignalSetToFindThree)),

    [BSignal] = ordsets:to_list(ordsets:del_element(DSignal, ordsets:subtract(FourSignals, OneSignals))),

    [FiveSignals] = lists:filter(fun(Elem) -> ordsets:is_element(BSignal, Elem) end, LengthFiveSignals),
    [ZeroSignals] = lists:filter(fun(Elem) -> not ordsets:is_element(DSignal, Elem) end, LengthSixSignals),
    [TwoSignals] = lists:filter(fun(Elem) -> (Elem =/= FiveSignals) and (Elem =/= ThreeSignals) end, LengthFiveSignals),
    [SixSignals] = lists:filter(fun(Elem) -> (Elem =/= NineSignals) and (Elem =/= ZeroSignals) end, LengthSixSignals),

    #{
      ZeroSignals => $0,
      OneSignals => $1,
      TwoSignals => $2,
      ThreeSignals => $3,
      FourSignals => $4,
      FiveSignals => $5,
      SixSignals => $6,
      SevenSignals => $7,
      EightSignals => $8,
      NineSignals => $9
     }.

output_values_to_number(OutputValues, SignalsToNumberMap) ->
    NumberStr = [maps:get(ordsets:from_list(OutputValue), SignalsToNumberMap) || OutputValue <- OutputValues],
    list_to_integer(NumberStr).

part_two(Lines) ->
    FoldFun = fun(Elem, Acc) ->
                      [SignalPatterns, OutputValues] = string:split(Elem, " | ", all),
                      SignalPatternsList = string:split(SignalPatterns, " ", all),
                      OutputValuesList = string:split(OutputValues, " ", all),

                      SignalsToNumberMap = build_signals_to_number_map(SignalPatternsList),
                      Acc + output_values_to_number(OutputValuesList, SignalsToNumberMap)
              end,
    Sum = lists:foldl(FoldFun, 0, Lines),
    io:format("~p~n", [Sum]).

main([InputFilename]) ->
    {ok, BinaryContents} = file:read_file(InputFilename),
    StrContents = string:trim(unicode:characters_to_list(BinaryContents), trailing, "\n"),
    Lines = string:split(StrContents, "\n", all),

    part_one(Lines),
    part_two(Lines).
