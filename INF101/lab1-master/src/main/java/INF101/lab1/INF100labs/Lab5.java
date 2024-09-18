package INF101.lab1.INF100labs;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;

/**
 * Implement the methods removeThrees, uniqueValues and addList.
 * These programming tasks was part of lab5 in INF100 fall 2022. You can find them here: https://inf100h22.stromme.me/lab/5/
 */
public class Lab5 {
    
    public static void main(String[] args) {
        // Call the methods here to test them on different inputs
        ArrayList<Integer> a1 = new ArrayList<>(Arrays.asList(1, 2, 3,1));
        ArrayList<Integer> b1 = new ArrayList<>(Arrays.asList(4, 2, -3));
        addList(a1, b1);
        System.out.println(a1);

    }

    public static ArrayList<Integer> multipliedWithTwo(ArrayList<Integer> list) {
        ArrayList<Integer> newList = new ArrayList<>();
        list.forEach((n) -> newList.add(n*2));
        return newList;
    }

    public static ArrayList<Integer> removeThrees(ArrayList<Integer> list) {
        list.removeAll(Collections.singleton(3));
        return list;
    }

    public static ArrayList<Integer> uniqueValues(ArrayList<Integer> list) {
        HashSet set = new HashSet<>(list);
        ArrayList<Integer> newList = new ArrayList<>(set);
        return newList;
    }

    public static void addList(ArrayList<Integer> a, ArrayList<Integer> b) {
        for(int i = 0; i < a.size();i++){
            try {
                a.set(i,a.get(i) + b.get(i));
            }catch (IndexOutOfBoundsException e){

            }
        }
    }

}