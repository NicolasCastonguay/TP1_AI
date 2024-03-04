import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        SixQueens game = new SixQueens(); // Initialisation du jeu
        Scanner scanner = new Scanner(System.in);
        int currentPlayer = 1; // Joueur 1 commence

        // Boucle tant que la condition de victoire n'est pas atteinte
        while (!game.checkWin()) {
            System.out.println("Joueur " + currentPlayer + ", entrez votre mouvement (format: add x y, move x y newy, remove x y):");
            String input = scanner.nextLine();
            String[] parts = input.split(" ");
            int row, col, newRow;

            // Traitement des commandes du joueur
            switch (parts[0]) {
                case "add":
                    row = Integer.parseInt(parts[1]);
                    col = Integer.parseInt(parts[2]);
                    if (game.addQueen(row, col)) {
                        System.out.println("Reine ajoutée avec succès.");
                    } else {
                        System.out.println("Mouvement invalide, essayez de nouveau.");
                        continue;
                    }
                    break;
                case "move":
                    row = Integer.parseInt(parts[1]);
                    col = Integer.parseInt(parts[2]);
                    newRow = Integer.parseInt(parts[3]);
                    if (game.moveQueen(row, col, newRow)) {
                        System.out.println("Reine déplacée avec succès.");
                    } else {
                        System.out.println("Mouvement invalide, essayez de nouveau.");
                        continue;
                    }
                    break;
                case "remove":
                    row = Integer.parseInt(parts[1]);
                    col = Integer.parseInt(parts[2]);
                    game.removeQueen(row, col);
                    System.out.println("Reine retirée.");
                    break;
                default:
                    System.out.println("Commande inconnue, essayez de nouveau.");
                    continue;
            }

            // Changement de joueur après chaque tour
            currentPlayer = (currentPlayer == 1) ? 2 : 1;
        }

        // Annonce du gagnant
        System.out.println("Le jeu est terminé. Le joueur " + ((currentPlayer == 1) ? 2 : 1) + " a gagné!");
        scanner.close();
    }
}