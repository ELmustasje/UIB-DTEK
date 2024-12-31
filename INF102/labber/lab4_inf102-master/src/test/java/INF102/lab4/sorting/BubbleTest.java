package INF102.lab4.sorting;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import INF102.lab4.utils.Range;

public class BubbleTest {

    static ISort sortingAlgorithm;
    static List<Integer> numberList;

    @BeforeAll
    public static void setup() {
        sortingAlgorithm = new BubbleSort();
        numberList = Range.intRange(10000);
        Collections.shuffle(numberList);
    }
    
    @Test
    public void correctSorting() {
        assertFalse(isSorted(numberList));
        sortingAlgorithm.sort(numberList);
        assertTrue(isSorted(numberList));
    }

    private static <T> boolean isSorted(List<T> list) {
        return list.stream().sorted().collect(Collectors.toList()).equals(list);
    }
}
