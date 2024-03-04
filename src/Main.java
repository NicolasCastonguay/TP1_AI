//TIP To <b>Run</b> code, press <shortcut actionId="Run"/> or
// click the <icon src="AllIcons.Actions.Execute"/> icon in the gutter.
/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Main.java to edit this template
 */

/**
 *
 * @author nicolascastonguay
 */
import java.util.Scanner;
import com.mycompany.tp1_ai.SixQueens;

public class Main {
    public static void main(String[] args) {
        com.mycompany.tp1_ai.SixQueens game = new SixQueens();
        Scanner scanner = new Scanner(System.in);
        int currentPlayer = 1; // Joueur 1 commence

        while (!game.checkWin()) {
            System.out.println("Joueur " + currentPlayer + ", entrez votre mouvement (format: add x y, move x y newy, remove x y):");
            String input = scanner.nextLine();
            String[] parts = input.split(" ");
            int row, col, newRow;

            switch (parts[0]) {
                case "add":
                    row = Integer.parseInt(parts[1]);
                    col = Integer.parseInt(parts[2]);
                    if (game.addQueen(row, col)) {
                        System.out.println("Reine ajoutée avec succès.");
                    } else {
                        System.out.println("Mouvement invalide, essayez de nouveau.");
                        continue; // Ne pas changer de joueur si le mouvement est invalide
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
                        continue; // Ne pas changer de joueur si le mouvement est invalide
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
                    continue; // Ne pas changer de joueur si la commande est inconnue
            }

            // Changer de joueur
            currentPlayer = (currentPlayer == 1) ? 2 : 1;
        }

        // Annoncer le gagnant
        System.out.println("Le jeu est terminé. Le joueur " + ((currentPlayer == 1) ? 2 : 1) + " a gagné!");
        scanner.close();
    }
}