package INF102.lab1.main;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;

import INF102.lab1.triplicate.ITriplicate;
import INF102.lab1.triplicate.MyTriplicate;
import INF102.lab1.triplicate.TriplicateBruteForce;

/**
 * Records the time of different algorithms for determining if a list contains
 * (at least) three of a given element.
 * 
 * @author Sondre Bolland and Martin Vatshelle
 *
 */
public class Main {

  public static DecimalFormat formatter = new DecimalFormat("#, ###");

  public static void main(String[] args) {
    List<ITriplicate<Integer>> algorithmList = new ArrayList<>();
    TriplicateBruteForce<Integer> bruteForceAlgo = new TriplicateBruteForce<Integer>();
    algorithmList.add(bruteForceAlgo);
    algorithmList.add(new MyTriplicate<Integer>());
    // TODO: Add you own algorithms

    int listSize = 10;
    while (!algorithmList.isEmpty()) {
      List<List<Integer>> integerLists = generateLists(listSize);
      runAlgorithms(algorithmList, integerLists);
      listSize *= 10;
    }
  }

  /**
   * This method generates several lists, some with and some without triplicates.
   * 
   * @param listSize
   * @return
   */
  private static List<List<Integer>> generateLists(int listSize) {
    // Generate lists of integers with (and without) triplicates
    System.out.println("---Generating Integer Lists---");
    List<List<Integer>> integerLists = new ArrayList<>();
    int nLists = 10;
    for (int i = 0; i < nLists; i++) {
      if (i % 2 == 0)
        integerLists.add(generateList(true, listSize));
      else
        integerLists.add(generateList(false, listSize));
    }
    System.out.printf("%slists generated with %selements each.%n%n", formatter.format(nLists),
        formatter.format(listSize));
    return integerLists;
  }

  private static void runAlgorithms(List<ITriplicate<Integer>> algorithmList, List<List<Integer>> integerLists) {
    // Run containsThree of each algorithm on the generated lists.
    // Record time of each algorithm for every list
    List<ITriplicate<Integer>> slowAlgorithms = new ArrayList<>();
    System.out.println("---Processing Algorithms---");
    for (ITriplicate<Integer> algorithm : algorithmList) {
      long timeElapsedMicro = timeAlgorithm(algorithm, integerLists) / 1000;
      double timeElapsedSeconds = (timeElapsedMicro / 1000000.0);
      if (timeElapsedSeconds > 0.1)
        slowAlgorithms.add(algorithm);
      String algorithmName = algorithm.getClass().getSimpleName();
      System.out.printf("%-27s| time elapsed: %10d microseconds (%f seconds)%n", algorithmName, timeElapsedMicro,
          timeElapsedSeconds);
    }
    System.out.println('\n');
    algorithmList.removeAll(slowAlgorithms);
  }

  /**
   * Runs the given <code>algorithm</code> on several lists
   * <code>integerLists</code> to find any occurrence of a triplicate. Records the
   * time spent to find/not find triplicates in all the lists.
   * 
   * @param algorithm    findTriplicate algorithm
   * @param integerLists list of lists of integers to be searched for triplicates
   * @return long of nanoseconds spent
   */
  public static long timeAlgorithm(ITriplicate<Integer> algorithm, List<List<Integer>> integerLists) {
    long startTime = System.nanoTime();
    for (List<Integer> integerList : integerLists) {
      algorithm.findTriplicate(integerList);
    }
    long endTime = System.nanoTime();
    long timeElapsed = (endTime - startTime);
    return timeElapsed;
  }

  /**
   * Generate a list of all integers from 0 to <code>n</code>-1 in random order
   * If the parameter withTriplicate is true, a set of at least 3 indices are all
   * replaced
   * with a specific random number between 0 and n-1
   * 
   * @param n              integers to be generated
   * @param withTriplicate
   * @return list of integers generated
   */
  public static List<Integer> generateList(boolean withTriplet, int n) {
    Random rand = new Random();
    // Create list of all numbers in bound with random order
    ArrayList<Integer> integerList = new ArrayList<>(n);
    for (int i = 0; i < n; i++) {
      integerList.add(i);
    }

    // Add equal integers to the list
    if (withTriplet) {
      int numEquals = rand.nextInt(n);
      int randNum = rand.nextInt(n);
      for (int i = 0; i < numEquals; i++) {
        int randIdx = rand.nextInt(n - i);
        integerList.set(randIdx, integerList.get(n - i - 1));
        integerList.set(n - i - 1, randNum);
      }
    }
    Collections.shuffle(integerList);
    return integerList;
  }

}
