package labyrinth.model.board.items;

import java.util.Arrays;
import java.util.Iterator;

import grid.CellPosition;
import grid.GridCell;
import grid.GridDirection;

/**
 * Item to be placed on board
 */
public class BoardItem implements Iterable<GridCell<Character>> {

    private final LabyrinthTile tile;
    private CellPosition pos;

    public BoardItem(CellPosition pos, LabyrinthTile tile) {
        this.tile = tile;
        this.pos = pos;
    }

    /**
     * Enum representing the tile
     * @return tile of object
     */
    public LabyrinthTile getTile() {
        return this.tile;
    }

    /**
     * Position of object
     * @return position
     */
    public CellPosition getPos() {
        return this.pos;
    }

    /**
     * Creates a new, identical object, which has a position shifted in the direction given
     * @param dir
     * @return shifted item
     */
    public BoardItem shiftedBy(GridDirection dir) {
        CellPosition shiftedPos = new CellPosition(pos.row + dir.dx, pos.col + dir.dy);
        return new BoardItem(shiftedPos, tile);
    }

    public Iterator<GridCell<Character>> iterator() {
        GridCell<Character> cell = new GridCell<Character>(pos, tile.getSymbol());
        return Arrays.asList(cell).iterator();
    }

}
