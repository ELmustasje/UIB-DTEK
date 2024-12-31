package INF102.lab4.median;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;


public class MedianTest {

    private static final int UPPERBOUND = 10000;
    private static final int N_NUMBERS = 100001;
    private static final int N_LISTS = 100;
    static List<List<Double>> numberLists;
    static Random random = new Random();

    static IMedian naive;
    static IMedian quick;

    @BeforeAll
    public static void setup() {
        numberLists = new ArrayList<>();
        for (int i = 0; i < N_LISTS; i++) {
            List<Double> numbers = new ArrayList<>();
            for (int j = 0; j < N_NUMBERS; j++) {
                double randomDouble = random.nextDouble(UPPERBOUND);
                numbers.add(randomDouble);
            }
            numberLists.add(numbers);
        }

        naive = new NaiveMedian();
        quick = new QuickMedian(); 
    }

    /* ############### FUNCTIONALITY TESTS ############### */
    @Test
    public void correctMedianTest() {
        for (List<Double> list : numberLists) {
            double naiveMedian = naive.median(new ArrayList<>(list));
            double quickMedian = quick.median(new ArrayList<>(list));
            assertEquals(naiveMedian, quickMedian);
        }
    }
    @Test
    public void MyTest() {
        double naiveMedian = naive.median(new ArrayList<>(numberLists.get(0)));
        double quickMedian = quick.median(new ArrayList<>(numberLists.get(0)));
        assertEquals(naiveMedian, quickMedian);
    }

    /* ############### RUNTIME TESTS ############### */
	@Test
	public void fasterThanNaive() {
        long myTime = timeAlgorithm(quick);
        long benchMarkTime = timeAlgorithm(naive);
		
		if ((benchMarkTime/2) < myTime) {
			fail("Your algorithm was slower than the naive solution.");
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


}
