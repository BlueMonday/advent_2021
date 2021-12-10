#!/usr/bin/env escript
-mode(compile).

closing_to_open($)) -> $(;
closing_to_open($]) -> $[;
closing_to_open($}) -> ${;
closing_to_open($>) -> $<.

illegal_character_points($)) -> 3;
illegal_character_points($]) -> 57;
illegal_character_points($}) -> 1197;
illegal_character_points($>) -> 25137.

calc_error_score([H | T], Stack) ->
    case H of
        H when H == ${; H == $[; H == $(; H == $< ->
            calc_error_score(T, [H | Stack]);
        _ ->
            case length(Stack) of
                0 -> 0;
                _ ->
                    [StackH | StackT] = Stack,
                    ExpectedOpen = closing_to_open(H),
                    case ExpectedOpen == StackH of
                        true -> calc_error_score(T, StackT);
                        _ -> illegal_character_points(H)
                    end
            end
    end;
calc_error_score(_, _) ->
    0.

part_one(Lines) ->
    FoldFun = fun(Line, Acc) -> Acc + calc_error_score(binary_to_list(Line), []) end,
    ErrorScore = lists:foldl(FoldFun, 0, Lines),
    io:format("~p~n", [ErrorScore]).

open_to_closing($() -> $);
open_to_closing($[) -> $];
open_to_closing(${) -> $};
open_to_closing($<) -> $>.

find_completion_characters(Line) ->
    FoldFun = fun(Char, Stack) ->
                      case Char of
                          Char when Char == ${; Char == $[; Char == $(; Char == $< ->
                              [Char | Stack];
                          _ ->
                              case length(Stack) of
                                  0 -> Stack;
                                  _ ->
                                      [_StackH | StackT] = Stack,
                                      StackT
                              end
                      end
              end,
    Stack = lists:foldl(FoldFun, [], Line),
    [open_to_closing(Char) || Char <- Stack].

calc_completion_characters_score(ComplChars) ->
    FoldFun = fun(Char, Acc) ->
                      Points = case Char of
                                   $) -> 1;
                                   $] -> 2;
                                   $} -> 3;
                                   $> -> 4
                               end,
                      Acc * 5 + Points
              end,
    lists:foldl(FoldFun, 0, ComplChars).

part_two(Lines) ->
    FindIncompleteFun = fun(Line, Acc) ->
                      ErrorScore = calc_error_score(binary_to_list(Line), []),
                      case ErrorScore of
                          0 -> [binary_to_list(Line) | Acc];
                          _ -> Acc
                      end
              end,
    IncompleteLines = lists:reverse(lists:foldl(FindIncompleteFun, [], Lines)),

    ComplCharsList = [find_completion_characters(Line) || Line <- IncompleteLines],
    Scores = [calc_completion_characters_score(ComplChars) || ComplChars <- ComplCharsList],
    SortedScores = lists:sort(Scores),
    MiddleScore = lists:nth(round(length(SortedScores) / 2), SortedScores),
    io:format("~p~n", [MiddleScore]).

main([InputFilename]) ->
    {ok, BinaryContents} = file:read_file(InputFilename),
    Lines = binary:split(BinaryContents, <<"\n">>, [global, trim]),

    part_one(Lines),
    part_two(Lines).
