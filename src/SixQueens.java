
/**
 *
 * @author nicolascastonguay
 */
import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Variable;
public class SixQueens {
    private final int SIZE = 6; // Grandeur
    private int[][] board; // Plateau

    // Constructeur
    public SixQueens() {
        Query.hasSolution("consult('six_queens.pl')");
        Query.hasSolution("init_board(Board)");
    }

    // Verifie si une reine peut etre placee a la [ligne][colonne]
    public boolean isSafe(int row, int col) {
        String queryString = String.format("is_safe(Board, %d, %d", row + 1, col + 1);
        return Query.hasSolution(queryString);
    }

    public boolean addQueen(int row, int col) {
        if (row >= 0 && row < SIZE && col >= 0 && col < SIZE) {
            // Convertir le plateau de jeu en une liste
            String prologList = boardToList(board);
            // Construire la requête Prolog
            String query = String.format("add_queen(%s, %d, %d, NewBoard)", prologList, row + 1, col + 1);
            Map<String, Term>[] solutions = new Query(query).allSolutions();
            if (solutions.length > 0) {
                // Mettre à jour le plateau de jeu avec le nouveau plateau retourné par Prolog
                board = listToBoard(solutions[0].get("NewBoard"));
                return true; // Ajout réussi
            }
        }
        return false; // Ajout échoué
    }

    public boolean moveQueen(int currentRow, int currentCol, int newRow) {
        // Vérifier si les positions sont dans les limites du plateau
        if (currentRow >= 0 && currentRow < SIZE && currentCol >= 0 && currentCol < SIZE && newRow >= 0 && newRow < SIZE) {
            board[currentRow][currentCol] = 0; // Enlever temporairement la reine pour vérifier la sécurité
            if (isSafe(newRow, currentCol)) {
                board[newRow][currentCol] = 1; // Déplacer la reine si la nouvelle position est sûre
                return true; // Déplacement réussi
            } else {
                // Le déplacement n'est pas sûr, remettre la reine à sa position originale
                board[currentRow][currentCol] = 1;
            }
        }
        return false; // Déplacement échoué
    }

    public void removeQueen(int row, int col) {
        // Vérifier si la position est dans les limites du plateau
        if (row >= 0 && row < SIZE && col >= 0 && col < SIZE) {
            board[row][col] = 0; // Enlever une reine
        }
    }
    public boolean checkWin() {
        // Convert the Java board array into Prolog list format
        String prologBoard = boardToList(board);
        // Query Prolog to check if all queens are safe
        String query = String.format("all_queens_safe(%s)", prologBoard);
        return Query.hasSolution(query);
    }


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

    public int[][] listToBoard(Term prologList) {
        int[][] board = new int[SIZE][SIZE];
        Term[] lists = prologList.toTermArray();
        for (int i = 0; i < lists.length; i++) {
            Term[] innerList = lists[i].toTermArray();
            for (int j = 0; j < innerList.length; j++) {
                board[i][j] = innerList[j].intValue();
            }
        }
        return board;
    }
    // Affichage
    public void printBoard() {
        for (int i = 0; i < SIZE; i++) {
            for (int j = 0; j < SIZE; j++) {
                System.out.print(board[i][j] + " ");
            }
            System.out.println();
        }
    }
}
