:- use_module(library(jpl)).

board_size(6).

% Initialise le plateau de jeu
init_board(Board) :-
    length(Board, board_size),
    maplist(=(0), Board).


% Ajoute une reine à la colonne C et à la ligne R
add_queen(Board, R, C, NewBoard) :-
    nth1(RowIndex, Board, Row),
    replace_element(Row, C, 1, NewRow),
    replace_element(Board, RowIndex, NewRow, NewBoard).

% Déplace une reine de la colonne C et de la ligne R vers la nouvelle ligne
move_queen(Board, R, C, NewRow, NewBoard) :-
    nth1(R, Board, Row),
    replace_element(Row, C, 0, RowWithQueenRemoved),
    replace_element(Board, R, RowWithQueenRemoved, TempBoard),
    replace_element(TempBoard, NewRow, [(C,1)], NewBoard).

% Supprime une reine de la colonne C et de la ligne R
remove_queen(Board, R, C, NewBoard) :-
    nth1(RowIndex, Board, Row),
    replace_element(Row, C, 0, NewRow),
    replace_element(Board, RowIndex, NewRow, NewBoard).

% Vérifie si la position est valide pour une nouvelle reine
valid_position(Board, (R, C)) :-
    \+ member(Row, Board, RowIndex),
    \+ nth1(RowIndex, Row, 1),
    \+ threatened_by_diagonal(Board, (R, C)).

% Vérifie si une reine est menacée en diagonale
threatened_by_diagonal(Board, (R, C)) :-
    member(Row, Board, RowIndex),
    nth1(RowIndex, Row, 1),
    member(Col, Row, ColIndex),
    nth1(ColIndex, Row, 1),
    abs(R - RowIndex) =:= abs(C - ColIndex).

% Calcule la valeur heuristique pour le placement des reines sur le plateau
heuristic(Board, Heuristic) :-
    board_size(Size),
    findall((C, R), (between(1, Size, C), between(1, Size, R), valid_position(Board, (R, C))), ValidPositions),
    length(ValidPositions, Heuristic).

% Vérifie si la configuration actuelle du plateau est gagnante
check_win(Board) :-
    % Vérifie si toutes les reines sont placées en sécurité
    all_queens_safe(Board).

% Vérifie si toutes les reines sur le plateau sont en sécurité
all_queens_safe(Board) :-
    % Trouve toutes les positions des reines sur le plateau
    findall((R,C), (nth1(R, Board, Row), nth1(C, Row, 1)), Queens),
    % Vérifie si toutes les reines sont en sécurité
    all_safe(Queens, Board).

% Itère sur toutes les reines pour s'assurer que chacune n'est pas attaquée
all_safe([], _).
all_safe([(R,C)|Others], Board) :-
    % Vérifie si la reine à la position (R,C) est en sécurité
    valid_position(Board, (C, R)),
    % Passe à la vérification de la prochaine reine
    all_safe(Others, Board).

