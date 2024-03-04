:- use_module(library(jpl)).

board_size(6).

% Initialise le plateau de jeu
init_board(Board) :-
    length(Board, board_size),
    maplist(=(0), Board).


% Prédicats pour ajouter, déplacer et supprimer des reines
% Ajoute une reine à la colonne C et à la ligne R
add_queen([], C, R, [(C, R)]).
add_queen([(X, Y)|Queens], C, R, [(X, Y)|NewQueens]) :-
    add_queen(Queens, C, R, NewQueens).

% Déplace une reine de la colonne C et de la ligne R vers la nouvelle ligne
move_queen([(C, _)|Queens], C, R, NewRow, [(C, NewRow)|Queens]).
move_queen([(X, Y)|Queens], C, R, NewRow, [(X, Y)|NewQueens]) :-
    move_queen(Queens, C, R, NewRow, NewQueens).

% Supprime une reine de la colonne C et de la ligne R
remove_queen([(C, _)|Queens], C, R, Queens).
remove_queen([(X, Y)|Queens], C, R, [(X, Y)|NewQueens]) :-
    remove_queen(Queens, C, R, NewQueens).


% Vérifie si la position est valide pour une nouvelle reine
valid_position(Queens, (C, R)) :-
    \+ member((C, _), Queens),  % Pas de reine dans la même colonne
    \+ member((_, R), Queens),  % Pas de reine dans la même ligne
    \+ threatened_by_diagonal(Queens, (C, R)). % Pas de reine dans les mêmes diagonales

% Vérifie si une reine est menacée en diagonale
threatened_by_diagonal([], _).
threatened_by_diagonal([(X, Y)|Queens], (C, R)) :-
    \+ (abs(C - X) =:= abs(R - Y)),
    threatened_by_diagonal(Queens, (C, R)).

heuristic(Queens, Heuristic) :-
    findall((C, R), (board_size(Size), between(1, Size, C), between(1, Size, R), valid_position(Queens, (C, R))), ValidPositions),
    length(ValidPositions, Heuristic).  % Heuristic est le nombre de positions valides restantes

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

