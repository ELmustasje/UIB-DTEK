package INF102.lab3.peakElement;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import INF102.lab3.utils.Range;

public class Main {
    
    private static final int UPPERBOUND = 1000000;
    private static final int N_LISTS = 1000;
    private static List<List<Integer>> numberLists;

    private static DecimalFormat formatter = new DecimalFormat("#, ###");

    public static void main(String[] args) {
        initializeNumbers();

        IPeak iterative = new PeakIterative();
        IPeak recursive = new PeakRecursive();
        List<IPeak> algorithms = new ArrayList<>();
        algorithms.add(iterative);
        algorithms.add(recursive);

        System.out.printf("%slists generated with %selements each.%n%n", formatter.format(N_LISTS),
				formatter.format(UPPERBOUND));
        System.out.println("---Processing Algorithms---");
        for (IPeak algorithm : algorithms) {
            long timeElapsedMicro = timeAlgorithm(algorithm) / 1000;
			double timeElapsedSeconds = (timeElapsedMicro / 1000000.0);
			String algorithmName = algorithm.getClass().getSimpleName();
			System.out.printf("%-27s| time elapsed: %10d microseconds (%f seconds)%n", algorithmName, timeElapsedMicro,
					timeElapsedSeconds);
        }
    } 

    private static long timeAlgorithm(IPeak algorithm) {
        long startTime = System.nanoTime();
        for (List<Integer> numbers : numberLists) {
            algorithm.peakElement(numbers);
        }
        long endTime = System.nanoTime();
		long timeElapsed = (endTime - startTime);
        return timeElapsed;
    }

    private static void initializeNumbers() {
        numberLists = new ArrayList<>();
        for (int i = 0; i < N_LISTS-2; i++) {
            List<Integer> numbers = Range.range(UPPERBOUND);
            Collections.shuffle(numbers);
            numberLists.add(numbers);
        }
        numberLists.add(Range.range(UPPERBOUND));
        List<Integer> numbers = Range.range(UPPERBOUND);
        Collections.reverse(numbers);
        numberLists.add(numbers);
    }
}
