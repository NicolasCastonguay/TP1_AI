import org.jpl7.Query;
import org.jpl7.Term;
import java.util.Map;

public class SixQueens {
    private final int SIZE = 6; // Taille du plateau
    private int[][] board; // Représentation du plateau

    // Constructeur
    public SixQueens() {
        // Consulte le fichier Prolog pour initialiser l'environnement Prolog
        Query.hasSolution("consult('six_queens.pl')");
        // Initialise le plateau dans Prolog
        Query.hasSolution("init_board(Board)");
    }

    // Prédicat pour ajouter une reine
    public void addQueen(int row, int col) {
        // Corrigez la chaîne de requête Prolog
        String queryString = String.format("add_queen(Board, %d, %d, NewBoard)", row + 1, col + 1);
        Query.hasSolution(queryString);
        // Met à jour le plateau en Java
        board[row][col] = 1;
    }

    // Déplace une reine de la colonne C et de la ligne R vers une nouvelle ligne
    public void moveQueen(int oldRow, int oldCol, int newRow) {
        // Vérifie d'abord si la nouvelle position est sûre
        if (isSafe(newRow, oldCol)) {
            // Corrigez la chaîne de requête Prolog
            String queryString = String.format("move_queen(Board, %d, %d, %d, NewBoard)", oldCol + 1, oldRow + 1, newRow + 1);
            Query.hasSolution(queryString);
            // Met à jour le plateau en Java
            board[oldRow][oldCol] = 0; // Supprime la reine de l'ancienne position
            board[newRow][oldCol] = 1; // Place la reine dans la nouvelle position
        } else {
            System.out.println("Impossible de déplacer la reine à cette position.");
        }
    }

    // Prédicat pour supprimer une reine
    public void removeQueen(int row, int col) {
        // Corrigez la chaîne de requête Prolog
        String queryString = String.format("remove_queen(Board, %d, %d, NewBoard)", row + 1, col + 1);
        Query.hasSolution(queryString);
        // Met à jour le plateau en Java
        board[row][col] = 0;
    }

    // Vérifie si une reine peut être placée aux coordonnées données
    public boolean isSafe(int row, int col) {
        // Corrigez la chaîne de requête Prolog
        String queryString = String.format("valid_position(Board, (%d, %d))", col + 1, row + 1);
        return Query.hasSolution(queryString);
    }

    // Vérifie si la position est gagnante
    public boolean checkWin() {
        String prologBoard = boardToList(board); // Convertit le plateau en liste Prolog
        String query = String.format("check_win(%s)", prologBoard);
        return Query.hasSolution(query);
    }

    // Convertit le plateau en liste Prolog
    public String boardToList(int[][] board) {
        StringBuilder listBuilder = new StringBuilder("[");
        for (int i = 0; i < board.length; i++) {
            listBuilder.append("[");
            for (int j = 0; j < board[i].length; j++) {
                listBuilder.append(board[i][j]);
                if (j < board[i].length - 1) {
                    listBuilder.append(",");
                }
            }
            listBuilder.append("]");
            if (i < board.length - 1) {
                listBuilder.append(",");
            }
        }
        listBuilder.append("]");
        return listBuilder.toString();
    }

    // Convertit une liste Prolog en plateau
    // Convertit une liste Prolog en plateau
    public int[][] listToBoard(Term prologList) {
        int[][] board = new int[SIZE][SIZE]; // Initialise un nouveau plateau
        Term[] lists = prologList.toTermArray();
        for (int i = 0; i < lists.length; i++) {
            Term innerList = lists[i];
            Term[] innerArray = innerList.toTermArray();
            for (int j = 0; j < innerArray.length; j++) {
                board[i][j] = innerArray[j].intValue();
            }
        }
        return board;
    }

    // Get the next move with the lowest heuristic value
    public int[] getNextMove() {
        int[] nextMove = new int[2];
        int minHeuristic = Integer.MAX_VALUE;

        // Iterate over all possible moves and calculate their heuristic value
        for (int row = 0; row < SIZE; row++) {
            for (int col = 0; col < SIZE; col++) {
                if (isSafe(row, col)) {
                    int heuristic = calculateHeuristic(row, col);
                    if (heuristic < minHeuristic) {
                        minHeuristic = heuristic;
                        nextMove[0] = row;
                        nextMove[1] = col;
                    }
                }
            }
        }
        return nextMove;
    }

    // Calculate the heuristic value for the given move
    private int calculateHeuristic(int row, int col) {
        String prologList = boardToList(board); // Convert the board to a Prolog list
        String query = String.format("heuristic(%s, [(%d, %d)], Heuristic)", prologList, row + 1, col + 1);
        Query q = new Query(query);
        if (q.hasSolution()) {
            return q.oneSolution().get("Heuristic").intValue();
        }
        return Integer.MAX_VALUE; // Default to maximum value if no solution found
    }

    public void printBoard() {
        for (int i = 0; i < SIZE; i++) {
            for (int j = 0; j < SIZE; j++) {
                if (board[i][j] == 1) {
                    System.out.print("Q "); // Queen present at this position
                } else {
                    System.out.print("_ "); // Empty position
                }
            }
            System.out.println(); // Move to the next row
        }
    }
}