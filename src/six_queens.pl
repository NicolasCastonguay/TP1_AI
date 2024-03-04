:- use_module(library(jpl)).

board_size(6).

% Initialise le plateau de jeu
init_board(Board) :-
    length(Board, board_size),
    maplist(=(0), Board).

% Vérifie si une reine est en sécurité
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

% Ajoute une reine sur le plateau
add_queen(Board, Row, Col, NewBoard) :-
    % Assure que le mouvement est dans les limites du plateau et que la case est libre
    valid_position(Row, Col),
    nth1(Row, Board, OldRow),
    replace(OldRow, Col, 1, NewRow),
    replace(Board, Row, NewRow, NewBoard).

% Déplace une reine d'une ligne à une autre dans la même colonne
move_queen(Board, CurrentRow, CurrentCol, NewRow, NewBoard) :-
    % Vérifie si le mouvement est valide et sûr
    valid_move(Board, CurrentRow, CurrentCol, NewRow),
    update_board(Board, CurrentRow, CurrentCol, NewRow, NewBoard).

% Retire une reine d'une position spécifique
remove_queen(Board, Row, Col, NewBoard) :-
    replace(Board, Row, Col, 0, NewBoard).

% Vérifie si la configuration actuelle du plateau est gagnante
check_win(Board) :-
    % Vérifie si toutes les reines sont placées en sécurité
    all_queens_safe(Board).

% Vérifie si toutes les reines sur le plateau sont en sécurité
all_queens_safe(Board) :-
    findall((R,C), (nth1(R, Board, Row), nth1(C, Row, 1)), Queens),
    all_safe(Queens, Board).

% Itère sur toutes les reines pour s'assurer que chacune n'est pas attaquée
all_safe([], _).
all_safe([(R,C)|Others], Board) :-
    is_safe(Board, R, C),
    all_safe(Others, Board).
