package INF102.lab2.list;

import java.util.Arrays;

public class ArrayList<T> implements List<T> {

  public static final int DEFAULT_CAPACITY = 10;

  private int n;

  private int currentCapacity;

  private Object elements[];

  public ArrayList() {
    elements = new Object[DEFAULT_CAPACITY];
    currentCapacity = DEFAULT_CAPACITY;
  }

  @Override
  public T get(int index) {
    if(index < 0 || index >= n) throw new IndexOutOfBoundsException();
    return (T) elements[index];
  }

  private void grow(){
    Object[] newElements = new Object[currentCapacity*2];
    for(int i = 0; i < n; i++) newElements[i] = elements[i];
    elements = newElements;
    currentCapacity = currentCapacity*2;
  }

  @Override
  public void add(int index, T element) {
    if(index < 0 || index > n) throw new IndexOutOfBoundsException();
    if(n == currentCapacity) grow();
    for(int i = n; i > index; i--) elements[i] = elements[i-1];
    elements[index] = element;
    n++;
  }

  @Override
  public int size() {
    return n;
  }

  @Override
  public boolean isEmpty() {
    return n == 0;
  }

  @SuppressWarnings("unchecked")
  @Override
  public String toString() {
    StringBuilder str = new StringBuilder(n * 3 + 2);
    str.append("[");
    for (int i = 0; i < n; i++) {
      str.append((T) elements[i]);
      if (i != n - 1)
        str.append(", ");
    }
    str.append("]");
    return str.toString();
  }
}
