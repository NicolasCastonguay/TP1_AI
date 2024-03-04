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

    // Vérifie si une reine peut être placée aux coordonnées données
    public boolean isSafe(int row, int col) {
        // Corrigez la chaîne de requête Prolog
        String queryString = String.format("is_safe(Board, %d, %d)", row + 1, col + 1);
        return Query.hasSolution(queryString);
    }

    // Ajoute une reine si c'est possible
    public boolean addQueen(int row, int col) {
        if (row >= 0 && row < SIZE && col >= 0 && col < SIZE) {
            String prologList = boardToList(board); // Convertit le plateau en liste Prolog
            String query = String.format("add_queen(%s, %d, %d, NewBoard)", prologList, row + 1, col + 1);
            Map<String, Term>[] solutions = new Query(query).allSolutions();
            if (solutions.length > 0) {
                board = listToBoard(solutions[0].get("NewBoard")); // Met à jour le plateau
                return true;
            }
        }
        return false;
    }

    // Déplace une reine
    public boolean moveQueen(int currentRow, int currentCol, int newRow) {
        if (currentRow >= 0 && currentRow < SIZE && currentCol >= 0 && currentCol < SIZE && newRow >= 0 && newRow < SIZE) {
            // Effectuez la vérification en utilisant Prolog pour la cohérence
            removeQueen(currentRow, currentCol); // Retire la reine de sa position actuelle
            if (isSafe(newRow, currentCol)) {
                addQueen(newRow, currentCol); // Ajoute la reine à la nouvelle position
                return true;
            } else {
                addQueen(currentRow, currentCol); // Remet la reine à sa position originale si le déplacement n'est pas sûr
            }
        }
        return false;
    }

    // Enlève une reine
    public void removeQueen(int row, int col) {
        if (row >= 0 && row < SIZE && col >= 0 && col < SIZE) {
            board[row][col] = 0; // Retire la reine du plateau
        }
    }

    // Vérifie si toutes les reines sont en sécurité
    public boolean checkWin() {
        String prologBoard = boardToList(board); // Convertit le plateau en liste Prolog
        String query = String.format("all_queens_safe(%s)", prologBoard);
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
    public int[][] listToBoard(Term prologList) {
        int[][] board = new int[SIZE][SIZE]; // Initialise un nouveau plateau
        Term[] lists = prologList.toTermArray();
        for (int i = 0; i < lists.length; i++) {
            Term[] innerList = lists[i].toTermArray();
            for (int j = 0; j < innerList.length; j++) {
                board[i][j] = innerList[j].intValue();
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