package INF102.lab4.median;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class NaiveMedian implements IMedian {

    @Override
    public <T extends Comparable<T>> T median(List<T> list) {
        List<T> listCopy = new ArrayList<>(list); // Method should not alter list
        
        Collections.sort(listCopy);
        int mid = listCopy.size() / 2;
        return listCopy.get(mid);
    }
    

}
