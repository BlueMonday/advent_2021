#!/usr/bin/env escript
-mode(compile).

parse_column([Elem | Elems], Row) ->
    parse_column(Elems, [{binary_to_integer(Elem), false} | Row]);
parse_column(_, Row) ->
    lists:reverse(Row).

build_board_map([UnparsedRow | UnparsedRows], Board) ->
    UnparsedRowStr = string:trim(binary_to_list(UnparsedRow)),
    UnparsedColumns = re:split(UnparsedRowStr, "\s+"),
    Row = parse_column(UnparsedColumns, []),
    build_board_map(UnparsedRows, [Row | Board]);
build_board_map(_, Board) ->
    lists:reverse(Board).

build_board_maps([UnparsedBoard | UnparsedBoards], Boards) ->
    UnparsedRows = binary:split(UnparsedBoard, <<"\n">>, [global, trim]),
    Board = build_board_map(UnparsedRows, []),
    build_board_maps(UnparsedBoards, [Board | Boards]);
build_board_maps(_, Boards) ->
    lists:reverse(Boards).

set_number_in_board(Number, [Row | Rows], ProcessedRows) ->
    Row1 = lists:keyreplace(Number, 1, Row, {Number, true}),
    set_number_in_board(Number, Rows, [Row1 | ProcessedRows]);
set_number_in_board(_, _, ProcessedRows) ->
    lists:reverse(ProcessedRows).

set_number(Number, [Board | Boards], ProcessedBoards) ->
    Board1 = set_number_in_board(Number, Board, []),
    set_number(Number, Boards, [Board1 | ProcessedBoards]);
set_number(_, _, Boards) ->
    %% Order of boards doesn't matter so lets not reverse for now.
    Boards.

check_board_horizontal([Row | Rows]) ->
    AllFun = fun(Elem) ->
                     {_, Set} = Elem,
                     Set == true
             end,
    case lists:all(AllFun, Row) of
        true -> true;
        _ -> check_board_horizontal(Rows)
    end;
check_board_horizontal(_) ->
    false.

check_board_vertical(Board, Index) ->
    AllFun = fun(Row) ->
                     {_, Set} = lists:nth(Index, Row),
                     Set == true
             end,
    case lists:all(AllFun, Board) of
        true -> true;
        _ ->
            BoardLength = length(Board),
            case Index == BoardLength of
                true -> false;
                false -> check_board_vertical(Board, Index + 1)
            end
    end.

calculate_score(Number, Board) ->
    FoldFun = fun(Row, Acc) ->
                      Sum = lists:foldl(fun(Elem, Acc1) -> {ElemNumber, Set} = Elem, case Set of false -> Acc1 + ElemNumber; true -> Acc1 end end, 0, Row),
                      Sum + Acc
              end,
    Sum = lists:foldl(FoldFun, 0, Board),
    Number * Sum.

check_boards_winner(Number, [Board | Boards]) ->
    case check_board_horizontal(Board) of
        true -> {ok, calculate_score(Number, Board)};
        _ -> case check_board_vertical(Board, 1) of
                 true -> {ok, calculate_score(Number, Board)};
                 false -> check_boards_winner(Number, Boards)
             end
    end;
check_boards_winner(_, _) ->
    %% no winner?
    false.


play_bingo([Number | Numbers], Boards) ->
    Boards1 = set_number(Number, Boards, []),
    case check_boards_winner(Number, Boards1) of
        {ok, Score} -> Score;
        _ -> play_bingo(Numbers, Boards1)
    end;
play_bingo(_, _) ->
    true.

part_one(Parts) ->
    NumbersBinary = lists:nth(1, Parts),
    Numbers = lists:map(fun binary_to_integer/1, binary:split(NumbersBinary, <<",">>, [global])),

    PartsLength = length(Parts),
    UnparsedBoards = lists:sublist(Parts, 2, PartsLength),
    Boards = build_board_maps(UnparsedBoards, []),

    Score = play_bingo(Numbers, Boards),
    io:format("~p~n", [Score]).

eliminate_winner_board(Number, [Board | Boards], Boards1) ->
    case check_board_horizontal(Board) of
        true -> eliminate_winner_board(Number, Boards, Boards1);
        _ -> case check_board_vertical(Board, 1) of
                 true -> eliminate_winner_board(Number, Boards, Boards1);
                 false -> eliminate_winner_board(Number, Boards, [Board | Boards1])
             end
    end;
eliminate_winner_board(_, _, Boards1) ->
    Boards1.

play_bingo_last_winner([Number | Numbers], Boards) ->
    Boards1 = set_number(Number, Boards, []),
    Boards2 = eliminate_winner_board(Number, Boards1, []),
    Boards2Length = length(Boards2),
    case Boards2Length == 0 of
        true -> calculate_score(Number, lists:nth(1, Boards1));
        _ -> play_bingo_last_winner(Numbers, Boards2)
    end;
play_bingo_last_winner(_, _) ->
    true.

part_two(Parts) ->
    NumbersBinary = lists:nth(1, Parts),
    Numbers = lists:map(fun binary_to_integer/1, binary:split(NumbersBinary, <<",">>, [global])),

    PartsLength = length(Parts),
    UnparsedBoards = lists:sublist(Parts, 2, PartsLength),
    Boards = build_board_maps(UnparsedBoards, []),

    Score = play_bingo_last_winner(Numbers, Boards),
    io:format("~p~n", [Score]).

main([InputFilename]) ->
    {ok, BinaryContents} = file:read_file(InputFilename),
    Parts = binary:split(BinaryContents, <<"\n\n">>, [global, trim]),

    part_one(Parts),
    part_two(Parts).
