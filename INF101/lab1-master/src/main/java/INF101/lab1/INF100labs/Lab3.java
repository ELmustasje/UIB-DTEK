package INF101.lab1.INF100labs;

import java.awt.font.NumericShaper;
import java.time.temporal.ValueRange;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.stream.IntStream;

/**
 * Implement the methods multiplesOfSevenUpTo, multiplicationTable and crossSum.
 * These programming tasks was part of lab3 in INF100 fall 2022. You can find them here: https://inf100h22.stromme.me/lab/3/
 */
public class Lab3 {
    
    public static void main(String[] args) {
        // Call the methods here to test them on different inputs
        System.out.println(crossSum(212));

    }

    public static void multiplesOfSevenUpTo(int n) {
        int num = 7;
        while (num <= n){
            System.out.println(num);
            num += 7;
        }
    }

    public static void multiplicationTable(int n) {
        for (int i = 1; i <= n; ++i) {
            System.out.print(i + ": ");
            for (int j = 1; j <= n; ++j) {
                System.out.print((i * j) + " ");
            }
            System.out.println();
        }

    }

    public static int crossSum(int num) {
        String snum = String.valueOf(num);
        ArrayList<String> nums = new ArrayList<>(Arrays.asList(snum.split("")));
        int sum = 0;
        for(String a : nums){
            sum += Integer.parseInt(a);
        }
        return sum;
    }

}