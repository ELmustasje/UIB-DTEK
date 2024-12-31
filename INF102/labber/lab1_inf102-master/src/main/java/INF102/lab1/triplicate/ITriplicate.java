package INF102.lab1.triplicate;

import java.util.List;

public interface ITriplicate<T> {

	/**
	 * Find an element that occurs three times in <code>list</list>
	 * @return If the list contains three of the same element then return this element
	 * null if not
	 */
	public T findTriplicate(List<T> list);
}