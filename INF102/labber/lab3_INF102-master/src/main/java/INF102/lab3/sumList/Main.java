package INF102.lab3.sumList;

import java.util.ArrayList;
import java.util.List;

import INF102.lab3.utils.Range;

public class Main {

  static final long UPPERBOUND = 1000;
  static final List<Long> numbers = Range.range(UPPERBOUND);

  public static void main(String[] args) {
    ISum iterative = new SumIterative();
    ISum recursive = new SumRecursive();
    List<ISum> algorithms = new ArrayList<>();
    algorithms.add(iterative);
    // algorithms.add(recursive);

    System.out.println("---Processing Algorithms---");
    for (ISum algorithm : algorithms) {
      long timeElapsedMicro = timeAlgorithm(algorithm) / 1000;
      double timeElapsedSeconds = (timeElapsedMicro / 1000000.0);
      String algorithmName = algorithm.getClass().getSimpleName();
      System.out.printf("%-27s| time elapsed: %10d microseconds (%f seconds)%n", algorithmName, timeElapsedMicro,
          timeElapsedSeconds);
    }
  }

  private static long timeAlgorithm(ISum algorithm) {
    long startTime = System.nanoTime();
    algorithm.sum(numbers);
    long endTime = System.nanoTime();
    long timeElapsed = (endTime - startTime);
    return timeElapsed;
  }

}
