package INF101.lab1.INF100labs;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Implement the methods removeRow and allRowsAndColsAreEqualSum.
 * These programming tasks was part of lab7 in INF100 fall 2022. You can find
 * them here: https://inf100h22.stromme.me/lab/7/
 */

public class Lab7 {

    public static void main(String[] args) {
        // Call the methods here to test them on different inputs

    }

    public static void removeRow(ArrayList<ArrayList<Integer>> grid, int row) {
        grid.remove(row);
    }

    public static int arraySum(ArrayList<Integer> array){
        int sum = 0;
        for (int i : array){
            sum += i;
        }
        return sum;
    }
    public static int colSum(ArrayList<ArrayList<Integer>> grid,int index){
        int sum = 0;
        for(ArrayList<Integer> i : grid){
            sum += i.get(index);
        }
        return sum;
    }

    public static boolean allRowsAndColsAreEqualSum(ArrayList<ArrayList<Integer>> grid) {
        //rad
        int startRowSum = arraySum(grid.get(0));
        for(ArrayList<Integer> i : grid){
            if(arraySum(i) != startRowSum){
                return false;
            }
        }
        //col
        int startColSum = colSum(grid,0);
        int length = grid.get(0).size();
        for(int i = 0; i < length; i++){
            if (colSum(grid,i)!= startColSum){
                return false;
            }
        }
        return true;
    }
}