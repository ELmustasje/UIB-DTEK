package INF102.lab1.triplicateTest;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import INF102.lab1.triplicate.ITriplicate;
import INF102.lab1.triplicate.MyTriplicate;
import INF102.lab1.triplicate.TriplicateBruteForce;

/**
 * Functionality and empirical runtime tests for triplicate algorithm
 * 
 * @author Sondre Bolland
 */
public class TriplicateTest {

	/**
	 * Number of integers in generated list
	 */
	public static final int N_INTEGERS = 100;
	/**
	 * Lower bound of integers in generated list
	 */
	public static final int LOWERBOUND = 0;
	/**
	 * Upper bound of integers in generated list
	 */
	public static final int UPPERBOUND = N_INTEGERS * 10;
	/**
	 * List of integers with at least one element occurring three times
	 */
	static List<Integer> integerListWithTriplet;
	/**
	 * List of integers with no element occurring three times
	 */
	static List<Integer> integerListWithoutTriplet;
	/**
	 * Benchemark triplicate finder algorithm
	 */
	static ITriplicate<Integer> benchMarkAlgorithm = new TriplicateBruteForce<>();
	/**
	 * Your implementation of the triplicate algorithm
	 */
	static ITriplicate<Integer> myAlgorithm = new MyTriplicate<>();
	
	@BeforeAll
	public static void setup() {
		integerListWithTriplet = generateList(true);
		integerListWithoutTriplet = generateList(false);
	}

	/**
	 * Generate a list of <code>n</code> random integers between
	 * <code>lowerbound</code> and <code>upperbound</code>
	 * 
	 * @param n          integers to be generated
	 * @param lowerbound of integers generated
	 * @param upperbound of integers generated
	 * @return list of integers generated
	 */
	public static List<Integer> generateList(boolean withTriplet) {
		Random rand = new Random();
		// Create list of all numbers in bound with random order
		List<Integer> integerList = new ArrayList<>();
		for (int i = LOWERBOUND; i < UPPERBOUND; i++) {
			integerList.add(i);
		}
		Collections.shuffle(integerList);
	
		// Add three equal integers to the list
		if (withTriplet) {
			int randNum = rand.nextInt(UPPERBOUND - LOWERBOUND) + UPPERBOUND;
			List<Integer> randIndecies = new ArrayList<>();
			while (randIndecies.size() < 3) {
				int randIdx = rand.nextInt(N_INTEGERS);
				if (!randIndecies.contains(randIdx)) {
					integerList.set(randIdx, randNum);
					randIndecies.add(randIdx);
				}
			}
		}
		return integerList;
	}

	/**
	 * Find all elements in list that occur (at least) three times
	 * 
	 * @param list
	 * @return list of integers occurring three times in the input list
	 */
	public static List<Integer> getTripletOccurrences(List<Integer> list) {
		List<Integer> threes = new ArrayList<>();
		int n = list.size();
		for (int i = 0; i < n; i++) {
			for (int j = i + 1; j < n; j++) {
				for (int k = j + 1; k < n; k++) {
					Integer one = list.get(i);
					Integer two = list.get(j);
					Integer three = list.get(k);
					if (one.equals(two) && two.equals(three)) {
						threes.add(one);
					}
				}
			}
		}
		return threes;
	}

	/* ############### FUNCTIONALITY TESTS ############### */
	@Test
	public void knownThreeTest() {
		for (Integer element : getTripletOccurrences(integerListWithTriplet)) {
			Integer foundElement = myAlgorithm.findTriplicate(integerListWithTriplet);
			if (element.equals(foundElement))
				return;
		}
		fail();
	}

	@Test
	public void noKnownThreeTest() {
		Integer foundElement = myAlgorithm.findTriplicate(integerListWithoutTriplet);
		assertEquals(foundElement, null);
	}


	/* ############### RUNTIME TESTS ############### */
	@Test
	public void fasterThanBruteForce() {
		long benchMarkTime = timeAlgorithm(benchMarkAlgorithm);
		long myTime = timeAlgorithm(myAlgorithm);
		
		if ((benchMarkTime/10) < myTime) {
			fail("Your algorithm was too slow.");
		}
	}

	public long timeAlgorithm(ITriplicate<Integer> algorithm) {
		long startTime = System.nanoTime();
		algorithm.findTriplicate(integerListWithTriplet);
		algorithm.findTriplicate(integerListWithoutTriplet);
		long endTime = System.nanoTime();
		long timeElapsed = (endTime - startTime);
		return timeElapsed;
	}
	
}
