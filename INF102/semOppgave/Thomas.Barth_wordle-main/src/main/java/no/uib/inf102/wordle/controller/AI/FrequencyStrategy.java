package no.uib.inf102.wordle.controller.AI;

import no.uib.inf102.wordle.model.Dictionary;
import no.uib.inf102.wordle.model.word.WordleWord;
import no.uib.inf102.wordle.model.word.WordleWordList;

import java.util.Map;

/**
 * This strategy finds the word within the possible words that has the highest
 * expected number of green matches.
 */
public class FrequencyStrategy implements IStrategy {

    private Dictionary dictionary;
    private WordleWordList guesses;

    /**
     * Constructs a FrequencyStrategy with the given dictionary.
     *
     * @param dictionary The dictionary to use for word guesses.
     */
    public FrequencyStrategy(Dictionary dictionary) {
        this.dictionary = dictionary;
        reset();
    }

    /**
     * Makes a guess based on letter frequencies.
     * If feedback is provided, it updates the list of possible guesses.
     *
     * @param feedback The feedback from the previous guess.
     * @return The best guess word based on frequency.
     */
    @Override
    public String makeGuess(WordleWord feedback) {
        // Eliminate words based on feedback if available
        if (feedback != null) {
            guesses.eliminateWords(feedback);
        }

        // Calculate letter frequencies for remaining possible answers
        Map<Character, Integer>[] letterFrequencies = guesses.calculateLetterFrequencies(guesses.possibleAnswers());

        // Find the best guess based on letter frequencies
        String bestWord = guesses.calculateGuessBasedOnFrequency(guesses.possibleAnswers(), letterFrequencies);

        // Return the best guess word
        return bestWord;
    }

    /**
     * Resets the strategy, reinitializing the guesses from the dictionary.
     */
    @Override
    public void reset() {
        guesses = new WordleWordList(dictionary);
    }
}