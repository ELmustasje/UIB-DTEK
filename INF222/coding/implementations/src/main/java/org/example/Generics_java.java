package org.example;

public class Generics_java<T>{
  T x;
  Generics_java(T x){
    this.x = x;
  }
  public T get_thing(){
    return x;
  }
  public static <T> void show_array(T[] arr){
    for (T thing : arr){
      System.out.println(thing);
    }
  }
  public static <T> T get_first(T[] arr){
    return arr[0];
  }


}
