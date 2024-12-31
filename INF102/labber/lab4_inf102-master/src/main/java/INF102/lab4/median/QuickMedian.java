package INF102.lab4.median;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;

public class QuickMedian implements IMedian {

    private final Random random = new Random();

    public <T extends Comparable<T>> T median(List<T> list) {
        if (list == null || list.isEmpty()) {
            throw new IllegalArgumentException("List cannot be null or empty");
        }

        List<T> listCopy = new ArrayList<>(list); // Create a copy to avoid modifying the original list
        int left = 0;
        int right = list.size() - 1;
        int medianIndex = list.size() / 2;

        while (left <= right) {
            int pivotIndex = left + random.nextInt(right - left + 1);; // Select a better pivot
            int newPivotIndex = partition(listCopy, left, right, pivotIndex);
            if (newPivotIndex == medianIndex) {
                return listCopy.get(newPivotIndex);
            } else if (newPivotIndex > medianIndex) {
                right = newPivotIndex - 1;
            } else {
                left = newPivotIndex + 1;
            }
        }

        throw new IllegalStateException("Median not found");
    }

    private <T extends Comparable<T>> int partition(List<T> list, int left, int right, int pivotIndex) {
        T pivotValue = list.get(pivotIndex);
        Collections.swap(list, pivotIndex, right); // Move pivot to end
        int storeIndex = left;
        for (int i = left; i < right; i++) {
            if (list.get(i).compareTo(pivotValue) < 0) {
                Collections.swap(list, storeIndex, i);
                storeIndex++;
            }
        }
        Collections.swap(list, right, storeIndex); // Move pivot to its final place
        return storeIndex;
    }
}