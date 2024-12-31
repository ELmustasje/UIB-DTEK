package INF102.lab4.utils;

import java.util.ArrayList;
import java.util.List;

/**
 * Util class for range function from Python
 * 
 * @author Sondre Bolland
 */
public class Range {

    /* ############### Integer ############### */
    public static List<Integer> intRange(int lowerbound, int upperbound, int step) {
        return integerRangeGenerator(lowerbound, upperbound, step);
    }

    public static List<Integer> intRange(int lowerbound, int upperbound) {
        return intRange(lowerbound, upperbound, 1);
    }

    public static List<Integer> intRange(int upperbound) {
        return intRange(0, upperbound, 1);
    }

    private static List<Integer> integerRangeGenerator(int lowerbound, int upperbound, int step) {    
        List<Integer> numbers = new ArrayList<>();
        for (int i = lowerbound; i < upperbound; i++) {
            if (i % step == 0)
                numbers.add(i);
        }
        return numbers;
    }

    /* ############### Long ############### */
    public static List<Long> longRange(long lowerbound, long upperbound, long step) {
        return longRangeGenerator(lowerbound, upperbound, step);
    }

    public static List<Long> longRange(long lowerbound, long upperbound) {
        return longRange(lowerbound, upperbound, 1);
    }

    public static List<Long> longRange(long upperbound) {
        return longRange(0, upperbound, 1);
    }

    private static List<Long> longRangeGenerator(long lowerbound, long upperbound, long step) {    
        List<Long> numbers = new ArrayList<>();
        for (long i = lowerbound; i < upperbound; i++) {
            if (i % step == 0)
                numbers.add(i);
        }
        return numbers;
    }

    /* ############### Double ############### */
    public static List<Double> doubleRange(int lowerbound, int upperbound, int step) {
        return doubleRangeGenerator(lowerbound, upperbound, step);
    }

    public static List<Double> doubleRange(int lowerbound, int upperbound) {
        return doubleRange(lowerbound, upperbound, 1);
    }

    public static List<Double> doubleRange(int upperbound) {
        return doubleRange(0, upperbound, 1);
    }

    private static List<Double> doubleRangeGenerator(int lowerbound, int upperbound, int step) {    
        List<Double> numbers = new ArrayList<>();
        for (double i = lowerbound; i < upperbound; i++) {
            if (i % step == 0)
                numbers.add(i);
        }
        return numbers;
    }

}

