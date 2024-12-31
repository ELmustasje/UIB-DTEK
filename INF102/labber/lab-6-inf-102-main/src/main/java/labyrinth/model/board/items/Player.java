package labyrinth.model.board.items;

import grid.CellPosition;

public class Player extends BoardItem {

    public Player(CellPosition pos) {
        super(pos, LabyrinthTile.PLAYER);
    }

}
