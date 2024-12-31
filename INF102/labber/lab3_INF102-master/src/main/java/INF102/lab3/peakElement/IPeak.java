package INF102.lab3.peakElement;

import java.util.List;

public interface IPeak {
    
    /**
     * Find the peak element in the list of integers.
     * A peak element is one that is greater (or equal) to both the element on the left and the right.
     * @param numbers
     * @return peak element
     */
    int peakElement(List<Integer> numbers);

}
