# Lab3 - Recursion and Search

This lab contains three tasks. To pass this lab you must finish all tasks.
The tasks increase in difficulty.

## Task 1 - Sum List of Integers (warm up exercise)
Given a list of integers calculate the sum. <br></br>
`[4, 6, 9, 1, 43, 2]` The sum if this list is `4 + 6 + 9 + 1 + 43 + 2 = 65`.

In the repository you will find an algorithm for computing the sum of a list of integers with an iterative solution in `SumIterative.java`. <br></br>
**TODO: Implement a recursive algorithm for calculating the sum of a list of numbers.** You are not allowed to use `while` or `for`.

**Hint:**<br></br>
This solution should remove one element for each recursive call of the function until there are no more elements in the list.

Implement `SumRecursive`:
```java
public class SumRecursive implements ISum {

    @Override
    public long sum(List<Long> list) {
        // Implement me :)
    }
    
}
```

✅ Run `SumTest` to check your solution. The task is passed if all tests in `SumTest` pass.


## Task 2 - Peak Element
In a list a peak element is one that is greater (or equal) to both the element on the left and the right. <br></br>
`[2, 4, 5, 11, 5, 0, -1]` <br></br>
In this list **11** is a peak element because it is greater (or equal) to both **4** and **5**.

Note that there may be several peak elements in a list. <br></br>
`[1, 4, 0, 9, 5, 0, -1]`<br></br>
This list contains two peak elements: **4** and **9**.

When an element is located on the first or last index we only consider the one element next to it. <br></br>
`[8, 9, 10, 12, 15]` <br></br>
In this list **15** is a peak element. It is greater than **12** and there is no element to the right.

`[0, 0, 0, 0, 0, 0, 0]` <br></br>
In this list all elements are peak elements as each element has an element both left and right that is greater or equal to it.


In the repository you will find an algorithm for computing the *peak element* with an iterative solution in `PeakIterative.java`. <br></br>
**TODO: Implement a recursive algorithm for finding a peak element.** Your algorithm should find **one peak element**. There may be more in the list, but simply finding one is sufficient. You are not allowed to use `while` or `for`. <br></br>
Implement `PeakRecursive`:
```java
public class PeakRecursive implements IPeak {

    @Override
    public int peakElement(List<Integer> numbers) {
        // Implement me :)
    }
    
}
```
✅ Run `PeakTest` to check your solution. The task is passed if all tests in `PeakTest` pass.


## Task 3 - Number Guesser
Given a randomly generated number between `0` and `n` you are to guess the number by any algorithmic means.

The `RandomNumber` class generates a random number given a lower and upper bound. This number is hidden within the class.
The method `RandomNumber::guess` is a query method which you use to guess a number. The method returns:
 * 0 if the guess is correct
 * -1 if the guess was too low
 * 1 if the guess was too high

For every guess you make the count of total guesses increments. 

Implement a Guesser class which uses fewer guesses than the existing guesser class. The number of guesses should be conciderably lower than `RandomGuesser`.
```java
public class MyGeniusGuesser implements IGuesser {
  
    @Override
    public int findNumber(RandomNumber number) {
        // TODO: Implement me :)
        return 0;
    }
}
```
Remember to add your guesser to the list of guessers in `Main::main`.

## Expected output:
 ```
Number range: 0  - 10 000 
After 1 000 guessing games the guessers got the following average guessing counts:
---------------------------------------------------------------------------------
RandomGuesser:                    10 510  guesses
MyGeniusGuesser:                   ?????  guesses
```

✅ Run `NumberGuesserTest` to check your solution. The task is passed if all tests in `NumberGuesserTest` pass.


#### 🆘 When you have finished all three tasks you must submit on Codegrade 🆘


