

public class Main {
    public static void main(String[] args) {
        SixQueens game = new SixQueens(); // Initialisation du jeu

        // Use A* search algorithm to find the solution
        a_star_search(game);

        // Print the final board
        System.out.println("Final board:");
        game.printBoard();
    }

    // A* search algorithm
    public static void a_star_search(SixQueens game) {
        while (!game.checkWin()) {
            // Get the next move with the lowest heuristic value
            int[] nextMove = game.getNextMove();
            int row = nextMove[0];
            int col = nextMove[1];

            // Add the queen to the next move
            game.addQueen(row, col);
        }
    }
}