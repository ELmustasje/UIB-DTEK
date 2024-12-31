package labyrinth.model.board.items;

import java.util.List;
import java.util.Random;

import grid.CellPosition;
import labyrinth.model.board.LabyrinthBoard;

public class BoardPositionFactory {

    private static Random random = new Random();

    public static CellPosition randomPosition(LabyrinthBoard board) {
        List<CellPosition> openTiles = board.openTiles();
        int randomIndex = random.nextInt(openTiles.size());
        CellPosition pos = openTiles.get(randomIndex);
        return pos;
    }
}
