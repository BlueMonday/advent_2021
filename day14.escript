#!/usr/bin/env escript
-mode(compile).

parse_pair_insertion_rules(RawPairInsertionRules) ->
    RawPairInsertionRulesList = binary:split(RawPairInsertionRules, <<"\n">>, [global, trim]),
    lists:foldl(fun(PairInsertionRule, PairInsertionRules) ->
                        [Pair, Insertion] = binary:split(PairInsertionRule, <<" -> ">>),
                        PairInsertionRules#{ binary_to_list(Pair) => binary_to_list(Insertion)}
                end, #{}, RawPairInsertionRulesList).

polymer_step(Polymer, PairInsertionRules) ->
    Polymer1 = lists:foldl(fun(Index, Polymer1) ->
                                   Left = lists:nth(Index, Polymer),
                                   Right = lists:nth(Index + 1, Polymer),
                                   Pair = [Left, Right],
                                   #{ Pair := Insertion} = PairInsertionRules,
                                   Polymer1 ++ [Left] ++ Insertion
                           end, "", lists:seq(1, length(Polymer) - 1)),
    Polymer1 ++ [lists:nth(length(Polymer), Polymer)].

part_one(Parts) ->
    [PolymerTemplate, RawPairInsertionRules] = Parts,
    PairInsertionRules = parse_pair_insertion_rules(RawPairInsertionRules),

    Polymer = lists:foldl(fun(_, Polymer) ->
                                  polymer_step(Polymer, PairInsertionRules)
                          end, binary_to_list(PolymerTemplate), lists:seq(1, 10)),
    CharToCount = lists:foldl(fun(Char, CharToCount) ->
                                      Count = maps:get(Char, CharToCount, 0),
                                      CharToCount#{Char => Count + 1}
                              end, #{}, Polymer),
    {Max, Min} = maps:fold(fun(_, Value, {Max, Min}) ->
                                   {max(Value, Max), min(Value, Min)}
                           end, {0, infinity}, CharToCount),
    io:format("~p~n", [Max - Min]).

polymer_template_to_pair_count(PolymerTemplate) ->
    lists:foldl(fun(Index, PairToCount) ->
                        Left = lists:nth(Index, PolymerTemplate),
                        Right = lists:nth(Index + 1, PolymerTemplate),
                        Pair = [Left, Right],

                        Count = maps:get(Pair, PairToCount, 0),
                        PairToCount#{Pair => Count + 1}
                end, #{}, lists:seq(1, length(PolymerTemplate) - 1)).

polymer_step_smarter(PairToCount, PairInsertionRules) ->
    maps:fold(fun(Pair, Count, PairToCount1) ->
                      #{ Pair := Insertion} = PairInsertionRules,
                      [Left, Right] = Pair,
                      Pair1 = [Left] ++ Insertion,
                      Count1 = maps:get(Pair1, PairToCount1, 0),

                      Pair2 = Insertion ++ [Right],
                      Count2 = maps:get(Pair2, PairToCount1, 0),
                      PairToCount1#{Pair1 => Count1 + Count, Pair2 => Count2 + Count}
              end, #{}, PairToCount).

part_two(Parts) ->
    [PolymerTemplate, RawPairInsertionRules] = Parts,
    PairToCount = polymer_template_to_pair_count(binary_to_list(PolymerTemplate)),
    PairInsertionRules = parse_pair_insertion_rules(RawPairInsertionRules),

    PairToCount1 = lists:foldl(fun(_, PairToCount1) ->
                                       polymer_step_smarter(PairToCount1, PairInsertionRules)
                               end, PairToCount, lists:seq(1, 40)),
    CharToCount = maps:fold(fun(Pair, Count, CharToCount) ->
                                    [Left, _] = Pair,

                                    CountLeft = maps:get(Left, CharToCount, 0),
                                    CharToCount#{Left => CountLeft + Count}
                            end, #{}, PairToCount1),

    LastChar = binary:at(PolymerTemplate, byte_size(PolymerTemplate) - 1),
    #{LastChar := LastCharCount} = CharToCount,
    CharToCount1 = CharToCount#{LastChar => LastCharCount + 1},

    {Max, Min} = maps:fold(fun(_, Value, {Max, Min}) ->
                                   {max(Value, Max), min(Value, Min)}
                           end, {0, infinity}, CharToCount1),
    io:format("~p~n", [Max - Min]).

main([InputFilename]) ->
    {ok, BinaryContents} = file:read_file(InputFilename),
    Parts = binary:split(BinaryContents, <<"\n\n">>, [global, trim]),

    part_one(Parts),
    part_two(Parts).
