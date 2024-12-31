package INF102.lab3.numberGuesser;

import java.util.Random;

/**
 * Class to generate a random number to be guessed.
 * The actual number is hidden. One can query the class by the guess method,
 *  telling you whether the guessed number is correct (0), too low (-1) or too high (1).
 *
 * @author Sondre Bolland
 *
 */
public class RandomNumber {

	/**
	 * Randomly generated number to guess
	 */
	private int number;
	/**
	 * Lower bound of the generated number
	 */
	private int lowerbound;
	/**
	 * Upper bound of the generated number
	 */
	private int upperbound;
	/**
	 * Number of guesses (times the guess method has been called)
	 */
	private int guessCount;
	
	public RandomNumber(RandomNumber randomNumber) {
		this.lowerbound = randomNumber.lowerbound;
		this.upperbound = randomNumber.upperbound;

		this.number = randomNumber.number;
	}
	public int getNumber(){
		return this.number;
	}



	public RandomNumber(int lowerbound, int upperbound) {
		this.lowerbound = lowerbound;
		this.upperbound = upperbound;
		
		Random rand = new Random();
		number = rand.nextInt(upperbound-lowerbound) + lowerbound;
	}
	
	/**
	 * Query method to guess the number.
	 * 
	 * For each call on this method the count of total guesses increments.
	 * @param guessedNumber number that you are guessing
	 * @return 0 if <code>guessedNumber</code> number is correct 
	 * 		-1 if <code>guessedNumber</code> is lower than the correct number
	 * 		1 if <code>guessedNumber</code> is higher than the correct number
	 */
	public int guess(int guessedNumber) {
		int comparison = Integer.compare(guessedNumber, number);
		guessCount++;
		return comparison;
	}
	
	/**
	 * Get the current total number of guesses
	 * @return count of guesses
	 */
	public int getGuessCount() {
		return guessCount;
	}
	
	public int getLowerbound() {
		return lowerbound;
	}
	
	public int getUpperbound() {
		return upperbound;
	}
}
