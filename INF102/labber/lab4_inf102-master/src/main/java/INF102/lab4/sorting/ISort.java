package INF102.lab4.sorting;

import java.util.List;

public interface ISort {
    
    /**
     * Sort <code>list</code> smallest to largest element.
     * @param list to be sorted
     */
    public <T extends Comparable<T>> void sort(List<T> list);

}
