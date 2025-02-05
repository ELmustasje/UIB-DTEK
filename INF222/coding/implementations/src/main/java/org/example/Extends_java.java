package org.example;

public class Extends_java <T extends Number>{
    public static <T extends Number> double add_generics(T x, T y){
        if (x == null || y == null){
            throw new IllegalArgumentException("w");
        }
        return x.doubleValue() + y.doubleValue();
    }
}
