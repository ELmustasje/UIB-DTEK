package INF102.lab3.peakElement;

import java.util.List;

public class PeakIterative implements IPeak {

    @Override
    public int peakElement(List<Integer> numbers) {
        if (numbers.isEmpty())
            throw new IllegalArgumentException("Empty list");

        // Edge cases
        int n = numbers.size();
        if (n == 1)
            return numbers.get(0);
        
        if (numbers.get(0) > numbers.get(1))
            return numbers.get(0);

        if (numbers.get(n-1) > numbers.get(n-2))
            return numbers.get(n-1);

        // Iterate over list and find peak
        for (int i = 1; i < numbers.size()-1; i++) {
            int prev = numbers.get(i-1);
            int current = numbers.get(i);
            int next = numbers.get(i+1);
            if (isPeak(prev, current, next))
                return current;
        }
        throw new IllegalArgumentException("No peak element");
    }

    private boolean isPeak(int prev, int current, int next) {
        return current > prev && current > next;
    }
    
}
