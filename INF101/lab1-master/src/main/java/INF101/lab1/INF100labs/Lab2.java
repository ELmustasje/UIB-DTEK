package INF101.lab1.INF100labs;

import java.util.ArrayList;
import java.util.Arrays;

/**
 * Implement the methods findLongestWords, isLeapYear and isEvenPositiveInt.
 * These programming tasks was part of lab2 in INF100 fall 2023. You can find them here: https://inf100h22.stromme.me/lab/2/
 */
public class Lab2 {
    
    public static void main(String[] args) {
        // Call the methods here to test them on different inputs

    }

    public static void findLongestWords(String word1, String word2, String word3) {
        ArrayList<String> words = new ArrayList<>(Arrays.asList(word1,word2,word3));
        ArrayList<String> longestWords = new ArrayList<>();

        for (String word : words){
            if (longestWords.isEmpty()){
                longestWords.add(word);
                continue;
            }
            if (longestWords.get(0).length() < word.length()) {
                longestWords.clear();
                longestWords.add(word);
            } else if (longestWords.get(0).length() == word.length()) {
                longestWords.add(word);
            }
        }

        for(String word : longestWords){
            System.out.println(word);
        }
    }

    public static boolean isLeapYear(int year) {
        if(year%4 == 0){
            if(year%100 == 0){
                if (year%400==0){
                    return true;
                }
                return false;
            }
            return true;
        }
        return false;
    }

    public static boolean isEvenPositiveInt(int num) {
        return (num>=0 && num%2 == 0);
    }

}
