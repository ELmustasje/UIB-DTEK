package cellular;

import datastructure.GridDirection;
import datastructure.CellPosition;

/**
 * An Ant can walk straight, turn right or turn left
 * Otherwise they don't do much.
 * Each Ant is in a position on a Grid and walks in a direction
 * 
 * @author Martin Vatshelle - martin.vatshelle@uib.no
 */
public class Ant {

	private CellPosition pos;
	private GridDirection dir;

	/**
	 * Constructs a new Ant in a position walking in a direction.
	 * Note that using {@link GridDirection#CENTER} produces an Ant that
	 * does not move.
	 * 
	 * @param pos the location the Ant starts in.
	 * @param dir the direction the Ant is walking in.
	 */
	public Ant(CellPosition pos, GridDirection dir) {
		this.pos = pos;
		this.dir = dir;
	}

	/**
	 * Changes the direction 90 degrees counterclockwise
	 */
	public void turnLeft() {
		dir = dir.turnLeft();
	}

	/**
	 * Changes the direction 90 degrees clockwise
	 */
	public void turnRight() {
		dir = dir.turnRight();
	}

	/**
	 * Moves this Ant one step straight forward in the direction
	 * this Ant is facing.
	 */
	public void move() {
		pos = pos.getNeighbor(dir);
	}

	public CellPosition getLocation() {
		return pos;
	}

	public void setRow(int x) {
		setLocation(new CellPosition(x, pos.col()));
	}

	public void setCol(int y) {
		setLocation(new CellPosition(pos.row(), y));
	}

	public void setLocation(CellPosition loc) {
		this.pos = loc;
	}

	public Ant copy() {
		return new Ant(pos, dir);
	}

	public int getRow() {
		return pos.row();
	}

	public int getCol() {
		return pos.col();
	}

}
