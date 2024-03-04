:- use_module(library(jpl)).

board_size(6).

% Initialiser le plateau de jeu
init_board(Board) :-
    length(Board, board_size),
    maplist(=(0), Board).

%verifie si la reine est en securite
is_safe(Board, Row, Col) :-
    not(attack(Row, Col, Board, 1)).

attack(Row, Col, Board, Index) :-
    nth1(Index, Board, QRow),
    QCol is Index,
    (   Row = QRow;  % Même ligne
        Col = QCol;  % Même colonne
        abs(Row - QRow) =:= abs(Col - QCol)  % Diagonale
    ), !.

attack(Row, Col, Board, Index) :-
    length(Board, Length),
    Index < Length,
    NextIndex is Index + 1,
    attack(Row, Col, Board, NextIndex).

% Add a queen to the board
add_queen(Board, Row, Col, NewBoard) :-
    % Ensure the move is within board limits and the spot is unoccupied
    valid_position(Row, Col),
    nth1(Row, Board, OldRow),
    replace(OldRow, Col, 1, NewRow),
    replace(Board, Row, NewRow, NewBoard).

% Move a queen from one row to another in the same column
move_queen(Board, CurrentRow, CurrentCol, NewRow, NewBoard) :-
    % Check if the move is valid and safe
    valid_move(Board, CurrentRow, CurrentCol, NewRow),
    update_board(Board, CurrentRow, CurrentCol, NewRow, NewBoard).

% Remove a queen from a specific position
remove_queen(Board, Row, Col, NewBoard) :-
    replace(Board, Row, Col, 0, NewBoard).

% Check if the current board configuration is a winning one
check_win(Board) :-
    % Check if all queens are placed safely
    all_queens_safe(Board).

% Check if the current board configuration is a winning one
all_queens_safe(Board) :-
    findall((R,C), nth1(R, Board, Row), nth1(C, Row, 1), Queens),
    all_safe(Queens, Board).

% Iterates through all queens ensuring each is not under attack
all_safe([], _).
all_safe([(R,C)|Others], Board) :-
    is_safe(Board, R, C),
    all_safe(Others, Board).
