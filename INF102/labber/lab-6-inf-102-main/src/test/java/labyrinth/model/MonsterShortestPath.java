package labyrinth.model;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import grid.CellPosition;
import labyrinth.model.board.LabyrinthBoard;

public class MonsterShortestPath {

    
    @Test
    public void monsterMovementBoard1Test() {
        CellPosition playerStartPos = new CellPosition(2, 1);
        CellPosition monsterStartPos = new CellPosition(2, 3);
        LabyrinthModel model = new LabyrinthModel(LabyrinthBoard.TEST_BOARD1, playerStartPos, monsterStartPos);

        int movesRequired = 4;
        for (int i = 0; i < movesRequired; i++) {
            assertEquals(GameState.ACTIVE_GAME, model.getGameState());
            model.moveMonster(); 
        }
        assertEquals(GameState.GAME_OVER, model.getGameState());
    }

    @Test
    public void monsterMovementBoard2Test() {
        CellPosition playerStartPos = new CellPosition( 7, 17);
        CellPosition monsterStartPos = new CellPosition(18, 1);
        LabyrinthModel model = new LabyrinthModel(LabyrinthBoard.TEST_BOARD2, playerStartPos, monsterStartPos);

        int movesRequired = 27;
        for (int i = 0; i < movesRequired; i++) {
            assertEquals(GameState.ACTIVE_GAME, model.getGameState());
            model.moveMonster(); 
        }
        assertEquals(GameState.GAME_OVER, model.getGameState());
    }

    @Test
    public void monsterMovementBoard3Test() {
        CellPosition playerStartPos = new CellPosition( 18, 1);
        CellPosition monsterStartPos = new CellPosition(1, 18);
        LabyrinthModel model = new LabyrinthModel(LabyrinthBoard.TEST_BOARD3, playerStartPos, monsterStartPos);

        int movesRequired = 34;
        for (int i = 0; i < movesRequired; i++) {
            assertEquals(GameState.ACTIVE_GAME, model.getGameState());
            model.moveMonster(); 
        }
        assertEquals(GameState.GAME_OVER, model.getGameState());
    }

    @Test
    public void monsterMovementBoard4Test() {
        CellPosition playerStartPos = new CellPosition( 0, 17);
        CellPosition monsterStartPos = new CellPosition(19, 0);
        LabyrinthModel model = new LabyrinthModel(LabyrinthBoard.TEST_BOARD4, playerStartPos, monsterStartPos);

        int movesRequired = 36;
        for (int i = 0; i < movesRequired; i++) {
            assertEquals(GameState.ACTIVE_GAME, model.getGameState());
            model.moveMonster(); 
        }
        assertEquals(GameState.GAME_OVER, model.getGameState());
    }

}
