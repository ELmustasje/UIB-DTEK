package org.example;

public class Main {
    public static void main(String[] args) {
        Integer[] arr = {1, 2, 3, 4, 5};
        Double[] arr2 = {1.2, 2.1, 3.4, 4.5, 5.1};
        Generics_java.show_array(arr);
        Generics_java.show_array(arr2);
        Integer a = Generics_java.get_first(arr);
        System.out.println(a);

        Generics_java<Integer> int_holder = new Generics_java<>(1);
        Generics_java<Double> double_holder = new Generics_java<>(1.2);
        Extends_java.add_generics(int_holder.get_thing(),double_holder.get_thing());

        Integer b = 10;
        Double g = 1.2;
        double c = Extends_java.add_generics(g,b);
        System.out.println(b);
    }
}