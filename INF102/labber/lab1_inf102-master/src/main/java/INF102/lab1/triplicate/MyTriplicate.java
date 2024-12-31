package INF102.lab1.triplicate;

import java.util.HashMap;
import java.util.List;

public class MyTriplicate<T> implements ITriplicate<T> {
  @Override
  public T findTriplicate(List<T> list) {
    // Implement me :)
    HashMap<T, Integer> counter = new HashMap<>();
    for (T i : list) {
      counter.put(i, counter.getOrDefault(i, 0) + 1);
      if (counter.get(i) == 3) {
        return i;
      }
    }
    return null;
  }

}
