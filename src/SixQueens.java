/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package com.mycompany.tp1_ai;

/**
 *
 * @author nicolascastonguay
 */

public class SixQueens {
    private final int SIZE = 6; // Grandeur
    private int[][] board; // Plateau

    // Constructeur
    public SixQueens() {
        this.board = new int[SIZE][SIZE];
        // Initialise le jeu, 0 indique une case vide
        for (int i = 0; i < SIZE; i++) {
            for (int j = 0; j < SIZE; j++) {
                this.board[i][j] = 0;
            }
        }
    }

    // Verifie si une reine peut etre placee a la [ligne][colonne]
    public boolean isSafe(int row, int col) {
        // Vérifier cette ligne sur le côté gauche
        for (int i = 0; i < col; i++) {
            if (board[row][i] == 1) {
                return false; // Conflit trouvé
            }
        }

        // Vérifier la diagonale supérieure à gauche
        for (int i = row, j = col; i >= 0 && j >= 0; i--, j--) {
            if (board[i][j] == 1) {
                return false; // Conflit trouvé
            }
        }

        // Vérifier la diagonale inférieure à gauche
        for (int i = row, j = col; j >= 0 && i < SIZE; i++, j--) {
            if (board[i][j] == 1) {
                return false; // Conflit trouvé
            }
        }

        // Vérifier cette ligne sur le côté droit
        for (int i = col + 1; i < SIZE; i++) {
            if (board[row][i] == 1) {
                return false; // Conflit trouvé
            }
        }

        // Vérifier la diagonale supérieure à droite
        for (int i = row - 1, j = col + 1; i >= 0 && j < SIZE; i--, j++) {
            if (board[i][j] == 1) {
                return false; // Conflit trouvé
            }
        }

        // Vérifier la diagonale inférieure à droite
        for (int i = row + 1, j = col + 1; i < SIZE && j < SIZE; i++, j++) {
            if (board[i][j] == 1) {
                return false; // Conflit trouvé
            }
        }

        // Si aucun conflit n'est trouvé, il est sûr de placer la reine
        return true;
    }

    public boolean addQueen(int row, int col) {
        // Vérifier si la position est dans les limites du plateau et si elle est sûre
        if (row >= 0 && row < SIZE && col >= 0 && col < SIZE && isSafe(row, col)) {
            board[row][col] = 1; // Placer une reine si c'est sûr
            return true; // Ajout réussi
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
        int queens = 0;
        for (int i = 0; i < SIZE; i++) {
            for (int j = 0; j < SIZE; j++) {
                if (board[i][j] == 1) {
                    queens++;
                }
            }
        }
        return queens == SIZE; // Gagné si 6 reines sont placées correctement
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
