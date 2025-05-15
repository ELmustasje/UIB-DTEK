package INF101.lab1.INF100labs;

import java.util.Scanner;

/**
 * Implement the methods task1, and task2.
 * These programming tasks was part of lab1 in INF100 fall 2023. You can find them here: https://inf100h22.stromme.me/lab/1/
 */
public class Lab1 {

    static Scanner sc = new Scanner(System.in);

    public static void main(String[] args) {
        // Call the methods here to test them on different inputs
        int a = 5;
        foo(a);
        System.out.println(a);

    }

    public static void foo(int a) {
        a += 10;
    }

    public static void task1() {
        System.out.println("Hei, det er meg, datamaskinen.\nHyggelig å se deg her.\nLykke til med INF101!");
    }

    public static void task2() {
        sc = new Scanner(System.in);
        String navn = readInput("Hva er ditt navn?");
        String adresse = readInput("Hva er din adresse?");
        String post = readInput("Hva er ditt postnummer og poststed?");

        System.out.println(navn+"s adresse er:");
        System.out.println();
        System.out.println(String.format("%s\n%s\n%s",navn,adresse,post));
    }

    /**
     * Reads input from console with given prompt
     * @param prompt
     * @return string input answer from user
     */
    public static String readInput(String prompt) {
        System.out.println(prompt);

        String userInput = sc.nextLine();
        return userInput;
    }

}
