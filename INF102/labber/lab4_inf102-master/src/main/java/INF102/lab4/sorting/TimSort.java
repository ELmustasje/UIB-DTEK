package INF102.lab4.sorting;

import java.util.Collections;
import java.util.List;

/**
 * Timsort is the sorting algorithm used by the Collections framework. It is a combination of merge sort and insertion sort.
 */
public class TimSort implements ISort {

    @Override
    public <T extends Comparable<T>> void sort(List<T> list) {
        Collections.sort(list); // This method performs timsort           
    }
    
}
