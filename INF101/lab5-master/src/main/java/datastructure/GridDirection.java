package datastructure;

import java.util.Arrays;

/**
 * This class represents the 8 different directions
 * (plus the direction of no movement) in a grid
 * which one can move by changing either x or y coordinate by at most 1
 */
public enum GridDirection {
	EAST(1, 0),
	NORTH(0, -1),
	WEST(-1, 0),
	SOUTH(0, 1), //
	NORTHEAST(1, -1),
	NORTHWEST(-1, -1),
	SOUTHWEST(-1, 1),
	SOUTHEAST(1, 1), //
	CENTER(0, 0);

	/**
	 * The four cardinal directions: {@link #NORTH}, {@link #SOUTH}, {@link #EAST},
	 * {@link #WEST}.
	 */
	public static final Iterable<GridDirection> FOUR_DIRECTIONS = Arrays.asList(
			EAST,
			NORTH,
			WEST,
			SOUTH
	);
	/**
	 * The eight cardinal and inter-cardinal directions: {@link #NORTH},
	 * {@link #SOUTH}, {@link #EAST}, {@link #WEST}, {@link #NORTHWEST},
	 * {@link #NORTHEAST}, {@link #SOUTHWEST}, {@link #SOUTHEAST}.
	 */
	public static final Iterable<GridDirection> EIGHT_DIRECTIONS = Arrays.asList(
			EAST,
			NORTHEAST,
			NORTH,
			NORTHWEST,
			WEST,
			SOUTHWEST,
			SOUTH,
			SOUTHEAST
	);
	/**
	 * The eight cardinal and inter-cardinal directions ({@link #EIGHT_DIRECTIONS}),
	 * plus {@link #CENTER}.
	 */
	public static final Iterable<GridDirection> NINE_DIRECTIONS = Arrays.asList(
			EAST,
			NORTHEAST,
			NORTH,
			NORTHWEST,
			WEST,
			SOUTHWEST,
			SOUTH,
			SOUTHEAST,
			CENTER
	);

	private final int dx;
	private final int dy;

	GridDirection(int dx, int dy) {
		this.dx = dx;
		this.dy = dy;
	}

	/**
	 * Finds the CellPosition adjacent in the given direction
	 * 
	 * @param pos position to start at
	 * @return neighbouring position
	 */
	public CellPosition getNeighbor(CellPosition pos) {
		return new CellPosition(pos.row() + dy, pos.col() + dx);
	}

	/**
	 * Returns the direction one get by rotating this direction
	 * 90 degrees to the left
	 */
	public GridDirection turnLeft() {
		return turnLeft45().turnLeft45();
	}

	public GridDirection turnLeft45() {
		return switch (this) {
			case NORTH -> GridDirection.NORTHWEST;
			case NORTHWEST -> GridDirection.WEST;
			case WEST -> GridDirection.SOUTHWEST;
			case SOUTHWEST -> GridDirection.SOUTH;
			case SOUTH -> GridDirection.SOUTHEAST;
			case SOUTHEAST -> GridDirection.EAST;
			case EAST -> GridDirection.NORTHEAST;
			case NORTHEAST -> GridDirection.NORTH;
			case CENTER -> GridDirection.CENTER;
		};
	}

	public GridDirection turnRight() {
		return turnRight45().turnRight45();
	}

	public GridDirection turnRight45() {
		return switch (this) {
			case NORTH -> GridDirection.NORTHEAST;
			case NORTHEAST -> GridDirection.EAST;
			case EAST -> GridDirection.SOUTHEAST;
			case SOUTHEAST -> GridDirection.SOUTH;
			case SOUTH -> GridDirection.SOUTHWEST;
			case SOUTHWEST -> GridDirection.WEST;
			case WEST -> GridDirection.NORTHWEST;
			case NORTHWEST -> GridDirection.NORTH;
			case CENTER -> GridDirection.CENTER;
		};
	}
}
