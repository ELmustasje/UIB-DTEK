package INF102.lab3.sumList;

import java.util.List;

public class SumIterative implements ISum {

    @Override
    public long sum(List<Long> list) {
        long sum = 0;
        for (Long number : list) {
            sum += number;
        }
        return sum;
    }
    
}
