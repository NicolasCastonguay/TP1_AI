

public class Main {
    public static void main(String[] args) {
        SixQueens game = new SixQueens(); // Initialisation du jeu
        a_star_search(game);
        System.out.println("Final board:");
        game.printBoard();
    }

    // A*
    public static void a_star_search(SixQueens game) {
        while (!game.checkWin()) {
            int[] nextMove = game.getNextMove();
            int row = nextMove[0];
            int col = nextMove[1];
            game.addQueen(row, col);
        }
    }
}