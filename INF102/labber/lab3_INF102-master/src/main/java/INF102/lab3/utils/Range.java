package INF102.lab3.utils;

import java.util.ArrayList;
import java.util.List;

/**
 * Util class for range function from Python
 * 
 * @author Sondre Bolland
 */
public class Range {

    /* ############### Integer Parameters ############### */
    public static List<Integer> range(int lowerbound, int upperbound, int step) {
        return integerRange(lowerbound, upperbound, step);
    }

    public static List<Integer> range(int lowerbound, int upperbound) {
        return range(lowerbound, upperbound, 1);
    }

    public static List<Integer> range(int upperbound) {
        return range(0, upperbound, 1);
    }

    private static List<Integer> integerRange(int lowerbound, int upperbound, int step) {    
        List<Integer> numbers = new ArrayList<>();
        for (int i = lowerbound; i < upperbound; i++) {
            if (i % step == 0)
                numbers.add(i);
        }
        return numbers;
    }

    /* ############### Long Parameters ############### */
    public static List<Long> range(long lowerbound, long upperbound, long step) {
        return longRange(lowerbound, upperbound, step);
    }

    public static List<Long> range(long lowerbound, long upperbound) {
        return range(lowerbound, upperbound, 1);
    }

    public static List<Long> range(long upperbound) {
        return range(0, upperbound, 1);
    }

    private static List<Long> longRange(long lowerbound, long upperbound, long step) {    
        List<Long> numbers = new ArrayList<>();
        for (long i = lowerbound; i < upperbound; i++) {
            if (i % step == 0)
                numbers.add(i);
        }
        return numbers;
    }

}
