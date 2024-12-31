package INF102.lab3.sumList;

import java.util.List;

public class SumRecursive implements ISum {

  @Override
  public long sum(List<Long> list) {
    // Implement me :)
    if (list.isEmpty())
      return 0;
    if (list.size() == 1)
      return list.get(0);
    else {
      list.set(0, list.get(0) + list.remove(list.size() - 1));
      return sum(list);
    }
  }
}
