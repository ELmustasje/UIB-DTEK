# Runtime Analysis
For each method of the tasks give a runtime analysis in Big-O notation and a description of why it has this runtime.

**If you have implemented new methods not listed you must add these as well, e.g. any helper methods. You need to show how you analyzed any methods used by the methods listed below.**

The runtime should be expressed using these three parameters:
   * `n` - number of words in the list allWords
   * `m` - number of words in the list possibleWords
   * `k` - number of letters in the wordleWords


## Task 1 - matchWord
* `WordleAnswer::matchWord`: O(k)
    * Checking the length of guess and answer takes O(1), and initializing arrays and maps takes O(k), where k is the number of letters in the answer.
    * Building the frequency map and identifying correct letters. This loop runs for each letter in the word (k times), taking O(k).
    * Identifying misplaced and wrong letters. This loop also runs for each letter in the word (k times), taking O(k).
    * Constructing the WordleWord object and returning it takes O(1), assuming the constructor runs in O(k) time.
    * So the dominant term here is O(k).

## Task 2 - EliminateStrategy
* `WordleWordList::eliminateWords`: O(m*k)
    * The method iterates through the list of possibleAnswers in reverse, which initially has m words.
    * For each word, it checks whether it is a possible match with the feedback received using the WordleWord.isPossibleWord method, which essentially involves comparing each character of the word to the feedback, taking O(k) time.
    * Removing elements from an ArrayList (especially when done from the end) is, on average, O(1) per operation.
    * eliminateWords is O(m * k) because it checks every word in the possibleAnswers list (m words) against feedback which requires checking each character in the word (k characters).


## Task 3 - FrequencyStrategy
* `FrequencyStrategy::makeGuess`: O(m*k)
    * *Uses the same eliminateWords as EliminateStrategy witch is O(m*k)
    * The method calculateLetterFrequencies is called with the list of possible answers. This involves iterating over each letter of each word and updating frequency counts in an array of maps.
      This step takes O(m * k) time because it processes each of the m words of length k.
    * calculateGuessBasedOnFrequency works by calculating the score for one word takes O(k) time, and this needs to be done for each of the m possible words, resulting in O(m * k).
    * combining all these to makehguess results in O(m * k) + O(m * k) + O(m * k) = O(m * k)



# Task 4 - Make your own (better) AI
For this task you do not need to give a runtime analysis. 
Instead, you must explain your code. What was your idea for getting a better result? What is your strategy?

*So first off I based my stragety on this paper: https://auction-upload-files.s3.amazonaws.com/Wordle_Paper_Final.pdf

My strategy combines elements from frequency analysis and entropy to optimize the guessing process in Wordle. The overall aim is to reduce the number of guesses needed by maximizing the information gained from each guess.
But if i used the entropy stategy on every guess the algorithm took to long time, so for the first guess i use letter frequencies to determine the best first guess.
The first guess is computed using the calculateFirstGuess method from WordleWordList, which prioritizes words without duplicate letters for maximum initial coverage.
For subsequent guesses, leverage entropy to maximize information gain.
Use the bestGuessBasedOnEntropy method, which computes the entropy for each possible word and selects the one that would, on average, reduce the space of possible answers the most.
Maintain and utilize the feedback from previous guesses using an ArrayList.
After each guess, eliminate words that are no longer possible based on the feedback received. This maintains the list of viable words.