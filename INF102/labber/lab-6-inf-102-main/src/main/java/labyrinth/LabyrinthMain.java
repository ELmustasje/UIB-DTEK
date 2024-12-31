package labyrinth;

import javax.swing.*;

import grid.CellPosition;
import labyrinth.controller.LabyrinthController;
import labyrinth.model.LabyrinthModel;
import labyrinth.model.board.LabyrinthBoard;
import labyrinth.view.LabyrinthView;

public class LabyrinthMain {
    public static final String WINDOW_TITLE = "INF102 Labyrinth";

    public static void main(String[] args) {
        LabyrinthModel model = new LabyrinthModel(LabyrinthBoard.makeRandomBoard(20, 20));

        // Test boards
        //LabyrinthModel model = testBoard1Model();
        //LabyrinthModel model = testBoard2Model();
        //LabyrinthModel model = testBoard3Model();
        //LabyrinthModel model = testBoard4Model();
        
        LabyrinthView view = new LabyrinthView(model);
        new LabyrinthController(model, view);

        JFrame frame = new JFrame(WINDOW_TITLE);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        frame.setContentPane(view);

        frame.pack();
        frame.setVisible(true);
    }

    public static LabyrinthModel testBoard1Model() {
        CellPosition playerStartPos = new CellPosition(2, 1);
        CellPosition monsterStartPos = new CellPosition(2, 3);
        LabyrinthModel model = new LabyrinthModel(LabyrinthBoard.TEST_BOARD1, playerStartPos, monsterStartPos);
        return model;
    }

    public static LabyrinthModel testBoard2Model() {
        CellPosition playerStartPos = new CellPosition( 7, 17);
        CellPosition monsterStartPos = new CellPosition(18, 1);
        LabyrinthModel model = new LabyrinthModel(LabyrinthBoard.TEST_BOARD2, playerStartPos, monsterStartPos);
        return model;
    }

    public static LabyrinthModel testBoard3Model() {
        CellPosition playerStartPos = new CellPosition( 18, 1);
        CellPosition monsterStartPos = new CellPosition(1, 18);
        LabyrinthModel model = new LabyrinthModel(LabyrinthBoard.TEST_BOARD3, playerStartPos, monsterStartPos);
        return model;
    }

    public static LabyrinthModel testBoard4Model() {
        CellPosition playerStartPos = new CellPosition( 0, 17);
        CellPosition monsterStartPos = new CellPosition(19, 0);
        LabyrinthModel model = new LabyrinthModel(LabyrinthBoard.TEST_BOARD4, playerStartPos, monsterStartPos);
        return model;
    }

    


    

}