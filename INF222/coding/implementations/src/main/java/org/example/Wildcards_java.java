package org.example;

import java.util.ArrayList;

public class Wildcards_java {
    public static void main(String[] args) {
        ArrayList<Number> numbers_arr = new ArrayList<>();
        ArrayList<Integer> integers_arr = new ArrayList<>();
        //numbers_arr = integers_arr;
        //not possible since Arraylist<Number> is not parent of Arraylist<Integer> even though integer is subtype of number
        ArrayList<? extends Number> numbers_arr2 = new ArrayList<>();
        numbers_arr2 = integers_arr;
        //works since the ? is a wildcard, and then you can put any subtype in the list
    }
}
