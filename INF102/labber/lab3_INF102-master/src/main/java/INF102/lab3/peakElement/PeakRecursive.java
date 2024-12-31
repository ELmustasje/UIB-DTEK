package INF102.lab3.peakElement;

import java.util.List;

public class PeakRecursive implements IPeak {

  @Override
  public int peakElement(List<Integer> numbers) {
    return findPeak(0, numbers.size() - 1, numbers);
  }

  public int findPeak(int low, int high, List<Integer> numbers) {
    int mid = low + (high - low) / 2;
    if (mid == numbers.size() - 1 || mid == 0 || numbers.get(mid) >= numbers.get(mid - 1) && numbers.get(mid) >= numbers.get(mid + 1)) {
      return numbers.get(mid);
    }
    if (numbers.get(mid) < numbers.get(mid - 1)) {
      return findPeak(low, mid - 1, numbers);
    }
    if (numbers.get(mid) < numbers.get(mid + 1)) {
      return findPeak(mid + 1, high, numbers);
    }
    throw new IllegalArgumentException("No peak element");
  }
}
