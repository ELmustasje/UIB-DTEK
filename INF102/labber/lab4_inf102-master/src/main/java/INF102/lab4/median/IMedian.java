package INF102.lab4.median;

import java.util.List;

public interface IMedian {
    
    public <T extends Comparable<T>> T median(List<T> list);
    
}
