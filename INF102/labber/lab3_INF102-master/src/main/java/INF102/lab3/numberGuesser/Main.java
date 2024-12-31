package INF102.lab3.numberGuesser;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


import java.text.DecimalFormat;

/**
 * Main class for running The Guessing Game.
 * A random number is generated given an upper and lower bound. 
 * A set of guessers are implemented which use different strategies
 * to guess the correct number. The guessers are evaluated on how many
 * guesses they have to use to find the correct number.
 * 
 * @author Sondre Bolland
 *
 */
public class Main {
	
    public static final int LOWERBOUND = 0;
	public static final int UPPERBOUND = 10000;
    public static final int N_GAMES = 1000;
	public static DecimalFormat formatter = new DecimalFormat("#, ###");


	public static void main(String[] args) {
		// Guessers
		List<IGuesser> guessers = new ArrayList<>();
		guessers.add(new RandomGuesser());
		guessers.add(new MyGeniusGuesser());
		
		Map<IGuesser, Integer> guessCounts = new HashMap<>();
		for (IGuesser guesser: guessers)
			guessCounts.put(guesser, 0);
		
		// Run n games
		for (int i = 0; i < N_GAMES; i++) {
			RandomNumber number = new RandomNumber(LOWERBOUND, UPPERBOUND);
			runGuessingGame(guessers, number, guessCounts);
		}
		
		// Print results
		System.out.printf("Number range: %s - %s%n",
				formatter.format(LOWERBOUND), formatter.format(UPPERBOUND));
		System.out.printf("After %s guessing games the guessers got the following"
				+ " average guessing counts:%n", formatter.format(N_GAMES));
		System.out.println("-----------------------------------------------------------------------------------");
		for (IGuesser guesser: guessers) {
			printResult(guesser, guessCounts.get(guesser), N_GAMES);
		}
	}
	
	/**
	 * Plays the guessing game <code>nGames</code> times
	 *  and tallies up the total number of guesses made by each guesser.
	 * 
	 * @param N_GAMES number of guessing games to be played
	 * @param guessers list of guessers participating in the guessing game
	 * @param number number object to be guessed
	 * @param guessCounts map of total guess counts per guesser
	 */
	public static void runGuessingGame(List<IGuesser> guessers,
		RandomNumber number, Map<IGuesser, Integer> guessCounts) {
		
		for (IGuesser guesser: guessers) {
			int guessCount = guessNumber(guesser, new RandomNumber(number));
			guessCounts.put(guesser, guessCounts.get(guesser) + guessCount);
		}
	}
	/**
	 * The given guesser guesses a number until the correct is found
	 * @param guesser
	 * @param randomNumber
	 * @return total number of guesses
	 */
	public static int guessNumber(IGuesser guesser, RandomNumber number) {
		int guessedNumber = guesser.findNumber(number);
		if (number.guess(guessedNumber) != 0)
			throw new IllegalStateException("The guessed number is not correct");
		
		int guessCount = number.getGuessCount()-1;
		return guessCount;
	}
	
	/**
	 * Prints how many guesses the given guesser used to correctly guess the number
	 * @param guesser
	 * @param guessCount
	 */
	public static void printResult(IGuesser guesser, int guessCount, int nGames) {
		String guesserName = guesser.getClass().getSimpleName();
		String formattedCount = formatter.format(guessCount/nGames);
		System.out.printf("%-25s %15s guesses%n", guesserName+":", formattedCount);
	}
	
}
