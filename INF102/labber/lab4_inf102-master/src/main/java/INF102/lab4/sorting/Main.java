package INF102.lab4.sorting;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import INF102.lab4.utils.Range;

public class Main {

    private static final int UPPERBOUND = 10000;
    private static final int N_LISTS = 100;
    private static List<List<Integer>> numberLists;

    private static DecimalFormat formatter = new DecimalFormat("#, ###");
    
    public static void main(String[] args) {
        initializeNumbers();

        ISort timSort = new TimSort();
        ISort bubbleSort = new BubbleSort();
        List<ISort> algorithms = new ArrayList<>();
        algorithms.add(timSort);
        algorithms.add(bubbleSort);

        System.out.printf("%slists generated with %selements each.%n%n", formatter.format(N_LISTS),
				formatter.format(UPPERBOUND));
        System.out.println("---Processing Algorithms---");
        for (ISort algorithm : algorithms) {
            long timeElapsedMicro = timeAlgorithm(algorithm) / 1000;
			double timeElapsedSeconds = (timeElapsedMicro / 1000000.0);
			String algorithmName = algorithm.getClass().getSimpleName();
			System.out.printf("%-27s| time elapsed: %10d microseconds (%f seconds)%n", algorithmName, timeElapsedMicro,
					timeElapsedSeconds);
        }
    } 

    private static long timeAlgorithm(ISort algorithm) {
        long startTime = System.nanoTime();
        for (List<Integer> list : numberLists) {
            algorithm.sort(new ArrayList<>(list));
        }
        long endTime = System.nanoTime();
		long timeElapsed = (endTime - startTime);
        return timeElapsed;
    }

    private static void initializeNumbers() {
        numberLists = new ArrayList<>();
        for (int i = 0; i < N_LISTS; i++) {
            List<Integer> numbers = Range.intRange(UPPERBOUND);
            Collections.shuffle(numbers);
            numberLists.add(numbers);
        }        
    }
 
} 
