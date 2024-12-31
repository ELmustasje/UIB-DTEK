# Lab4 - Sorting
This lab contains two sorting related tasks. Both must be completed to pass.

## Task 1 - Sorting Algorithm
Given a list of elements sort them from smallest to largest. 

In the repository you will find an algorithm for sorting a list using Java's Collections framework in `TimSort.java`. <br></br>
**TODO: Implement the [Bubble Sort](https://en.wikipedia.org/wiki/Bubble_sort) algorithm.**

```java
public class BubbleSort implements ISort {

    @Override
    public <T extends Comparable<T>> void sort(List<T> list) {
        // TODO: Implement me :)
    }

}
```

Tim Sort is a combination of Merge sort and Insertion sort, and has a runtime of `O(nlog(n))`. What is the runtime of your algorithm? Run `Main::main` to compare the two.

✅ Run `BubbleTest` to check your solution. The task is passed if all tests in `BubbleTest` pass.


## Task 2 - Calculating the Median
The median of a list is the middle element in regards to value. <br></br>
`[4, 6, 9, 1, 43, 2, 7]` The median of this list is `6`. <br></br>
**For this task we assume all lists to contain an odd number of elements. Hence, you do not need to consider if there are two elements in the middle of the list.**


In the repository you will find a naive algorithm to calculate the median in `NaiveMedian.java`. This solution runs in `O(nlog(n))`. <br></br>
**TODO: Implement an algorithm for calculating the median which runs in linear time (`O(n)`).**
```java
public class QuickMedian implements IMedian {

    @Override
    public <T extends Comparable<T>> T median(List<T> list) {
        List<T> listCopy = new ArrayList<>(list); // Method should not alter list
        
        // TODO: Implement me :)
        return null;
    }

}
```

✅ Run `MedianTest` to check your solution. The task is passed if all tests in `MedianTest` pass.

