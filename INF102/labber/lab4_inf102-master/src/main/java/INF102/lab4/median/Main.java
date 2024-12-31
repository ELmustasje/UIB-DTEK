package INF102.lab4.median;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class Main {

    private static final int UPPERBOUND = 10000;
    private static final int N_NUMBERS = 100001;
    private static final int N_LISTS = 100;
    private static List<List<Double>> numberLists;
    private static Random random = new Random();

    private static DecimalFormat formatter = new DecimalFormat("#, ###");
    
    public static void main(String[] args) {
        initializeNumbers();

        IMedian naive = new NaiveMedian();
        IMedian quick = new QuickMedian();
        List<IMedian> algorithms = new ArrayList<>();
        algorithms.add(quick);
        algorithms.add(naive);


        System.out.printf("%slists generated with %selements each.%n%n", formatter.format(N_LISTS),
				formatter.format(N_NUMBERS));
        System.out.println("---Processing Algorithms---");
        for (IMedian algorithm : algorithms) {
            long timeElapsedMicro = timeAlgorithm(algorithm) / 1000;
			double timeElapsedSeconds = (timeElapsedMicro / 1000000.0);
			String algorithmName = algorithm.getClass().getSimpleName();
			System.out.printf("%-27s| time elapsed: %10d microseconds (%f seconds)%n", algorithmName, timeElapsedMicro,
					timeElapsedSeconds);
        }
    } 

    private static long timeAlgorithm(IMedian algorithm) {
        long startTime = System.nanoTime();
        for (List<Double> list : numberLists) {
            algorithm.median(new ArrayList<>(list));
        }
        long endTime = System.nanoTime();
		long timeElapsed = (endTime - startTime);
        return timeElapsed;
    }

    private static void initializeNumbers() {
        numberLists = new ArrayList<>();
        for (int i = 0; i < N_LISTS; i++) {
            List<Double> numbers = new ArrayList<>();
            for (int j = 0; j < N_NUMBERS; j++) {
                double randomDouble = random.nextDouble(UPPERBOUND);
                numbers.add(randomDouble);
            }
            numberLists.add(numbers);
        }         
    }

} 
