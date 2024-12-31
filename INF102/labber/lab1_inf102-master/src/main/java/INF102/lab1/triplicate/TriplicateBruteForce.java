package INF102.lab1.triplicate;

import java.util.List;

/**
 * Checks for triplicate of given element by means of brute force.
 * 
 * Time complexity: O(n^3)
 * 
 * @author Sondre Bolland
 *
 */
public class TriplicateBruteForce<T> implements ITriplicate<T> {

	@Override
	public T findTriplicate(List<T> list) {
		int n = list.size();
		for (int i = 0; i < n; i++) {
			for (int j = i+1; j < n; j++) {
				for (int k = j+1; k < n; k++) {
					T one = list.get(i);
					T two = list.get(j);
					T three = list.get(k);
					if (one.equals(two) && two.equals(three)) {
						return one;
					}
				}
			}
		}
		return null;
	}

		
}
