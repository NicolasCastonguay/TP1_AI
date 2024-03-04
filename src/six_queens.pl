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

% Predicates for adding, moving, and removing queens
% Add a queen at column C and row R
add_queen([], C, R, [(C, R)]).
add_queen([(X, Y)|Queens], C, R, [(X, Y)|NewQueens]) :-
    add_queen(Queens, C, R, NewQueens).

% Move a queen from column C and row R to new_row
move_queen([(C, _)|Queens], C, R, NewRow, [(C, NewRow)|Queens]).
move_queen([(X, Y)|Queens], C, R, NewRow, [(X, Y)|NewQueens]) :-
    move_queen(Queens, C, R, NewRow, NewQueens).

% Remove a queen from column C and row R
remove_queen([(C, _)|Queens], C, R, Queens).
remove_queen([(X, Y)|Queens], C, R, [(X, Y)|NewQueens]) :-
    remove_queen(Queens, C, R, NewQueens).

% Heuristic function: Calculate the number of threatened pairs of queens
threatened_pairs(_, [], 0).
threatened_pairs((C, R), [(X, Y)|Queens], N) :-
    threatened_pairs((C, R), Queens, RestThreats),
    (C = X; R = Y; abs(C - X) =:= abs(R - Y)), % Check if (C, R) threatens (X, Y)
    N is RestThreats + 1.
threatened_pairs((C, R), [_|Queens], N) :-
    threatened_pairs((C, R), Queens, N).

% Calculate the heuristic value for a state
heuristic([], _, 0).
heuristic([(C, R)|Queens], NextQueen, Heuristic) :-
    heuristic(Queens, NextQueen, RestHeuristic),
    threatened_pairs((C, R), NextQueen, Threats),
    Heuristic is RestHeuristic + Threats.

% A* search algorithm
a_star_search(CurrentState, _, []) :- % Base case: current state is final
    heuristic(CurrentState, [], Heuristic),
    Heuristic =:= 0.
a_star_search(CurrentState, Visited, [NextMove|Moves]) :-
    select(NextMove, [(C, R)|CurrentState], NewState), % Select a queen to move
    \+ member(NewState, Visited), % Avoid loops
    heuristic(CurrentState, NewState, Heuristic),
    a_star_search(NewState, [NewState|Visited], Moves).

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

