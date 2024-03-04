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

    // Affiche le plateau
    public void printBoard() {
        for (int i = 0; i < SIZE; i++) {
            for (int j = 0; j < SIZE; j++) {
                System.out.print(board[i][j] + " "); // Affiche l'état actuel de chaque cellule du plateau
            }
            System.out.println(); // Passe à la ligne suivante après chaque rangée
        }
    }
}