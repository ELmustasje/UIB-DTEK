# Lab1 - Find Triplicate
Given a list of elements find an element that occurs (at least) three times.
For instance:
`[6, 81, 5, 2, 5, 10, 0, 23, 5]`
This list contains the triplicate: `5`.

The code currently includes a benchmark algorithm for doing this:
**TriplicateBruteForce**
  * Simple solution: Triple for-loop (brute force)
    * A brute force solutions simply tests all posibilities and selects one that fits the criteria. Most times there is a more efficient way of doing things.
  * Time Complexity: O(n<sup>3</sup>)

**TODO: Write a better/faster algorithm.**

## Task
Implement the ``ITriplicate`` interface. 
```java
public class MyTriplicate<T> implements ITriplicate<T> {

    @Override
    public T findTriplicate(List<T> list) {
        // Implement me :)
        return null;
    }
    
}
```
Write an algorithm which performs better than `TriplicateBruteForce`.
The algorithm must be functionally correct as well as faster than `TriplicateBruteForce`. Run `TriplicateTest` to check both requirements.

To see the empirical runtime of your solution add your algorithm/class to the list of algorithms/classes in `Main::main`. Run `Main` to see how fast (or slow) your solution is compared to `TriplicateBruteForce`. 

What time complexity does your algorithm have? Can you improve your time/write an even better algorithm?


## Expected output
```
---Generating Integer Lists---
10 lists generated with 10 000 elements each.

---Processing Algorithms---
TriplicateBruteForce        | time elapsed: 2826194217 microseconds (2826,194217 seconds)
MyTriplicate                | time elapsed: ?????????? microseconds (??????????? seconds)
```

✅ The lab is passed when you have submited on Codegrade and all the tests there pass.
