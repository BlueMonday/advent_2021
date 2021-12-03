#!/usr/bin/env escript
-mode(compile).

count_bits([Bit | Bits], Index, BitCount) ->
    {Zero, One} = maps:get(Index, BitCount, {0, 0}),
    Count = case Bit of
                $0 -> {Zero + 1, One};
                $1 -> {Zero, One + 1}
            end,
    BitCount1 = BitCount#{Index => Count},
    count_bits(Bits, Index + 1, BitCount1);
count_bits(_, _, BitCount) ->
    BitCount.

count_bits_in_lines([Line | Lines], BitCount) ->
    Bits = binary:bin_to_list(Line),
    BitCount1 = count_bits(Bits, 0, BitCount),
    count_bits_in_lines(Lines, BitCount1);
count_bits_in_lines(_, BitCount) ->
    BitCount.

common(Zero, One, most) when Zero > One ->
    zero;
common(Zero, One, most) when Zero =< One ->
    one;
common(Zero, One, least) when Zero > One ->
    one;
common(Zero, One, least) when Zero =< One ->
    zero.

convert_to_number(BitCount, Index, Value, Commonality) ->
    {Zero, One} = maps:get(Index, BitCount),
    Common = common(Zero, One, Commonality),
    Size = maps:size(BitCount),

    Value1 = if
                 Common == one ->
                     Value + math:pow(2, Size - 1 - Index);
                 true ->
                     Value
             end,

    if
        Index == Size - 1 ->
            Value1;
        true ->
            convert_to_number(BitCount, Index + 1, Value1, Commonality)
    end.

part_one(Lines) ->
    BitCount = count_bits_in_lines(Lines, #{}),

    Gamma = convert_to_number(BitCount, 0, 0, most),
    Epsilon = convert_to_number(BitCount, 0, 0, least),

    io:format("~p~n", [Gamma * Epsilon]).

find_number(Lines, Index, Commonality) ->
    BitCount = count_bits_in_lines(Lines, #{}),
    {Zero, One} = maps:get(Index, BitCount),
    Common = common(Zero, One, Commonality),

    FilterFun = fun(Elem) ->
                        Bit = binary:at(Elem, Index),
                        if
                            Bit == $1, Common == one ->
                                true;
                            Bit == $0, Common == zero ->
                                true;
                            true ->
                                false
                        end

                end,
    Lines1 = lists:filter(FilterFun, Lines),
    SizeLines1 = length(Lines1),
    SizeBits = maps:size(BitCount),

    if
        SizeLines1 == 1; Index == SizeBits - 1 ->
            lists:nth(1, Lines1);
        true ->
            find_number(Lines1, Index + 1, Commonality)
    end.

%% I could have reused this between part 1 and part 2 if I had done part 1
%% differently... Too lazy to refactor :(.
binary_bit_string_to_value(BinaryBitString, Index, Value) ->
    Size = byte_size(BinaryBitString),
    Bit = binary:at(BinaryBitString, Index),

    Value1 = if
                 Bit == $1 ->
                     Value + math:pow(2, Size - 1 - Index);
                 true ->
                     Value
             end,
    if
        Index == Size - 1 ->
            Value1;
        true ->
            binary_bit_string_to_value(BinaryBitString, Index + 1, Value1)
    end.

part_two(Lines) ->
    OxygenBinaryBitString = find_number(Lines, 0, most),
    CarbonBinaryBitString = find_number(Lines, 0, least),

    Oxygen = binary_bit_string_to_value(OxygenBinaryBitString, 0, 0),
    Carbon = binary_bit_string_to_value(CarbonBinaryBitString, 0, 0),

    io:format("~p~n", [Oxygen * Carbon]).

main([InputFilename]) ->
    {ok, BinaryContents} = file:read_file(InputFilename),
    Lines = binary:split(BinaryContents, <<"\n">>, [global, trim]),

    part_one(Lines),
    part_two(Lines).
