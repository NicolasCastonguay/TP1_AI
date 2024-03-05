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
        String queryString = String.format("add_queen(Board, %d, %d, NewBoard)", row - 1, col - 1);
        Query.hasSolution(queryString);
        // Met à jour le plateau en Java
        board[row-1][col-1] = 1;
    }

    // Déplace une reine de la colonne C et de la ligne R vers une nouvelle ligne
    public void moveQueen(int oldRow, int oldCol, int newRow) {
        // Vérifie d'abord si la nouvelle position est sûre
        if (isSafe(newRow-1, oldCol-1)) {
            // Corrigez la chaîne de requête Prolog
            String queryString = String.format("move_queen(Board, %d, %d, %d, NewBoard)", oldCol - 1, oldRow - 1, newRow - 1);
            Query.hasSolution(queryString);
            // Met à jour le plateau en Java
            board[oldRow-1][oldCol-1] = 0; // Supprime la reine de l'ancienne position
            board[newRow-1][oldCol-1] = 1; // Place la reine dans la nouvelle position
        } else {
            System.out.println("Impossible de déplacer la reine à cette position.");
        }
    }

    // Prédicat pour supprimer une reine
    public void removeQueen(int row, int col) {
        // Corrigez la chaîne de requête Prolog
        String queryString = String.format("remove_queen(Board, %d, %d, NewBoard)", row - 1, col - 1);
        Query.hasSolution(queryString);
        // Met à jour le plateau en Java
        board[row-1][col-1] = 0;
    }

    // Vérifie si une reine peut être placée aux coordonnées données
    public boolean isSafe(int row, int col) {
        // Corrigez la chaîne de requête Prolog
        String queryString = String.format("valid_position(Board, (%d, %d))", col - 1, row - 1);
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

    // Obtient le prochain coup avec la plus faible valeur heuristique
    public int[] getNextMove() {
        int[] nextMove = new int[2];
        int minHeuristic = Integer.MAX_VALUE;
        for (int row = 0; row < SIZE; row++) {
            for (int col = 0; col < SIZE; col++) {
                if (isSafe(row, col)) {
                    String prologList = boardToList(board);
                    String query = String.format("heuristic(%s, [%d, %d], Heuristic)", prologList, row, col);
                    Map<String, Term> solution = Query.oneSolution(query);
                    if (solution != null) {
                        int heuristic = solution.get("Heuristic").intValue();
                        if (heuristic < minHeuristic) {
                            minHeuristic = heuristic;
                            nextMove[0] = row;
                            nextMove[1] = col;
                        }
                    }
                }
            }
        }
        return nextMove;
    }
}