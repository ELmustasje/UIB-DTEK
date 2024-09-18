package labyrinth;

import java.awt.Color;

import datastructure.Grid;
import datastructure.GridDirection;
import datastructure.IGrid;
import java.lang.Math;
import java.util.*;

import datastructure.CellPosition;
import labyrinth.gui.LabyrinthGUI;

public class Labyrinth implements ILabyrinth {

	private final IGrid<LabyrinthTile> tiles;
	public CellPosition playerPos;
	public double distToGold;

	boolean playerSet;

	public Labyrinth(IGrid<LabyrinthTile> tiles) throws LabyrinthParseException {
		if (tiles == null) {
			throw new IllegalArgumentException();
		}

		this.tiles = tiles;

		int numPlayers = 0;
		for (CellPosition pos : tiles.cellPositions()) {
			if (tiles.get(pos) == LabyrinthTile.PLAYER) {
				numPlayers++;
				playerPos = pos;
				playerSet = true;
			}
		}
		if (numPlayers != 1) {
			throw new LabyrinthParseException("Labyrinth created with " + numPlayers + " number of players!");
		}

		checkState(this);
	}

	public static void checkState(Labyrinth labyrinth) {
		boolean ok = !labyrinth.playerSet || labyrinth.isValidPos(labyrinth.playerPos);
		int numPlayers = 0;
		for (CellPosition pos : labyrinth.tiles.cellPositions()) {
			if (labyrinth.tiles.get(pos) == LabyrinthTile.PLAYER) {
				numPlayers++;
			}
		}
		if (labyrinth.playerSet) {
			ok &= numPlayers == 1;
		} else {
			ok &= numPlayers == 0;
		}
		if (!ok) {
			throw new IllegalStateException("bad object");
		}
	}

	@Override
	public LabyrinthTile getCell(CellPosition pos) {
		checkPosition(pos);

		return tiles.get(pos);
	}

	@Override
	public Color getColor(CellPosition pos) {
		if (!isValidPos(pos)) {
			throw new IllegalArgumentException("Location invalid");
		}

		return tiles.get(pos).getColor();
	}

	@Override
	public int numberOfRows() {
		return tiles.rows();
	}

	@Override
	public int getPlayerGold() {
		return 0;
	}

	@Override
	public int getPlayerHitPoints() {
		return 0;
	}

	@Override
	public int numberOfColumns() {
		return tiles.cols();
	}

	@Override
	public boolean isPlaying() {
		return playerSet;
	}

	private boolean isValidPos(CellPosition pos) {
		return pos.row() >= 0 && pos.row() < tiles.rows() //
				&& pos.col() >= 0 && pos.col() < tiles.cols();
	}

	private void checkPosition(CellPosition pos) {
		if (!isValidPos(pos)) {
			throw new IndexOutOfBoundsException("Row and column indices must be within bounds");
		}
	}

	@Override
	public void movePlayer(GridDirection d) throws MovePlayerException{
		// TODO: check pre-conditions
		if(!playerCanGo(d)){
			throw new MovePlayerException("BAD MOVE!");
		}
		CellPosition newPos = playerPos.getNeighbor(d);
		tiles.set(playerPos, LabyrinthTile.OPEN);
		playerPos = newPos;
		tiles.set(newPos, LabyrinthTile.PLAYER);
		checkState(this);
	}
	@Override
	public double distToGold(CellPosition pos){

		CellPosition currentPos = pos;
		CellPosition newPos;
		int grid = Math.max(tiles.cols(),tiles.rows());
		double closestGold = grid*grid+10;
		for(int x = -grid; x <= grid; x++){
			for(int y = -grid; y<=grid; y++){
				try {
					newPos = new CellPosition(currentPos.row() + x, currentPos.col() + y);
					Color c = tiles.get(newPos).getColor();
					if (c.equals(Color.yellow)){
						double dist = Math.sqrt(Math.pow(x,2)+Math.pow(y,2));
						if (closestGold > dist){
							closestGold = dist;
						}
					}
				}catch (IndexOutOfBoundsException e){

				}
			}
		}

		return closestGold;
	}

	GridDirection stuckDir = GridDirection.NORTH;
	int stuckMoves = 0;
	@Override
	public void aiMove(){
		double cDist = distToGold(playerPos);
		GridDirection closestDir = null;
		double closestDist = 1000000000;
		if(stuckMoves == 0) {
			if (playerCanGo(GridDirection.SOUTH) && distToGold(new CellPosition(playerPos.row() + 1, playerPos.col())) < cDist) {
				if (closestDir == null) {
					closestDir = GridDirection.SOUTH;
					closestDist = distToGold(new CellPosition(playerPos.row() + 1, playerPos.col()));
				} else if (closestDist > distToGold(new CellPosition(playerPos.row() + 1, playerPos.col()))) {
					closestDir = GridDirection.SOUTH;
					closestDist = distToGold(new CellPosition(playerPos.row() + 1, playerPos.col()));
				}
			}
			if (playerCanGo(GridDirection.NORTH) && distToGold(new CellPosition(playerPos.row() - 1, playerPos.col())) < cDist) {
				if (closestDir == null) {
					closestDir = GridDirection.NORTH;
					closestDist = distToGold(new CellPosition(playerPos.row() - 1, playerPos.col()));
				} else if (closestDist > distToGold(new CellPosition(playerPos.row() - 1, playerPos.col()))) {
					closestDir = GridDirection.NORTH;
					closestDist = distToGold(new CellPosition(playerPos.row() - 1, playerPos.col()));
				}
			}
			if (playerCanGo(GridDirection.WEST) && distToGold(new CellPosition(playerPos.row(), playerPos.col() - 1)) < cDist) {
				if (closestDir == null) {
					closestDir = GridDirection.WEST;
					closestDist = distToGold(new CellPosition(playerPos.row(), playerPos.col() - 1));
				} else if (closestDist > distToGold(new CellPosition(playerPos.row(), playerPos.col() - 1))) {
					closestDir = GridDirection.WEST;
					closestDist = distToGold(new CellPosition(playerPos.row(), playerPos.col() - 1));
				}
			}
			if (playerCanGo(GridDirection.EAST) && distToGold(new CellPosition(playerPos.row(), playerPos.col() + 1)) < cDist) {
				if (closestDir == null) {
					closestDir = GridDirection.EAST;
					closestDist = distToGold(new CellPosition(playerPos.row(), playerPos.col() + 1));
				} else if (closestDist > distToGold(new CellPosition(playerPos.row(), playerPos.col() + 1))) {
					closestDir = GridDirection.EAST;
					closestDist = distToGold(new CellPosition(playerPos.row(), playerPos.col() + 1));
				}
			}
		}
		//stuck
		if(closestDir == null){
			System.out.println("ai stuck, trying to find path");
			if(stuckMoves == 0){
				stuckMoves = 5;
			}
			if (stuckDir ==null){
				if(playerCanGo(GridDirection.NORTH)){
					closestDir = GridDirection.NORTH;
					if(stuckMoves < 3) {
						stuckDir = GridDirection.NORTH;
					}
				} else if (playerCanGo(GridDirection.EAST)) {
					closestDir = GridDirection.EAST;
					if(stuckMoves < 3) {
						stuckDir = GridDirection.NORTH;
					}
				} else if (playerCanGo(GridDirection.SOUTH)) {
					closestDir = GridDirection.SOUTH;
					if(stuckMoves < 3) {
					}
					stuckDir = GridDirection.SOUTH;
				} else if (playerCanGo(GridDirection.WEST)) {
					closestDir = GridDirection.WEST;
					if(stuckMoves < 3) {
						stuckDir = GridDirection.NORTH;
					}
				}
			}else {
				if(stuckMoves > 0){
					if(stuckDir == GridDirection.NORTH || stuckDir == GridDirection.SOUTH){
						if(playerCanGo(GridDirection.EAST)){
							closestDir = GridDirection.EAST;
							stuckDir = GridDirection.EAST;
						} else if (playerCanGo(GridDirection.WEST)) {
							closestDir = GridDirection.WEST;
							stuckDir = GridDirection.WEST;
						}
					} else if (stuckDir == GridDirection.EAST || stuckDir == GridDirection.WEST) {
						if(playerCanGo(GridDirection.NORTH)){
							closestDir = GridDirection.NORTH;
							stuckDir = GridDirection.NORTH;
						} else if (playerCanGo(GridDirection.SOUTH)) {
							closestDir = GridDirection.SOUTH;
							stuckDir = GridDirection.SOUTH;
						}
					}
					stuckMoves--;
				}
			}
		}

		//move player
		try {
			movePlayer(closestDir);
		}catch (MovePlayerException e){

		}

	}

	@Override
	public CellPosition getPlayerPos(){
		return playerPos;
	}
	@Override
	public boolean playerCanGo(GridDirection d) {
		if (d == null) {
			throw new IllegalArgumentException();
		}

		return playerCanGoTo(playerPos.getNeighbor(d));
	}


	/**
	 * This method checks if a player can move to a given location
	 * A player can not go to the location if there is a wall or
	 * if the location is outside the bounds of the grid.
	 * 
	 * @param pos the position
	 * @return true if player can move, false otherwise
	 */
	private boolean playerCanGoTo(CellPosition pos) {
		if (!isValidPos(pos)) {
			return false;
		}

		return tiles.get(pos) != LabyrinthTile.WALL;
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();

		for (int y = tiles.cols() - 1; y >= 0; y--) {
			for (int x = 0; x < tiles.rows(); x++) {
				sb.append(getSymbol(new CellPosition(x, y)));
			}
			sb.append("\n");
		}
		return sb.toString();
	}

	/**
	 * No bounds checking will be done for the given {@code pos}.
	 */
	private String getSymbol(CellPosition pos) {
		return String.valueOf(tiles.get(pos).getSymbol());
	}

	@Override
	public Iterable<CellPosition> coordinates() {
		return tiles.cellPositions();
	}
}
