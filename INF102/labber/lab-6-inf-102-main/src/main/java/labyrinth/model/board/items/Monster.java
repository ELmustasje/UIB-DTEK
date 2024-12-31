package labyrinth.model.board.items;

import grid.CellPosition;

public class Monster extends BoardItem {

    public Monster(CellPosition pos) {
        super(pos, LabyrinthTile.MONSTER);
    }

}