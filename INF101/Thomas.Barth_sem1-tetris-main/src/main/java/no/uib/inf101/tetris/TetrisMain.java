package no.uib.inf101.tetris;

import no.uib.inf101.tetris.controller.TetrisController;
import no.uib.inf101.tetris.model.TetrisBoard;
import no.uib.inf101.tetris.model.TetrisModel;
import no.uib.inf101.tetris.model.tetromino.RandomTetrominoFactory;
import no.uib.inf101.tetris.model.tetromino.TetrominoFactory;
import no.uib.inf101.tetris.view.TetrisView;

import javax.swing.*;

/**
 * The TetrisMain class is the entry point for the Tetris game application.
 * It sets up the game board, tetromino factory, model, view, and controller.
 * It also creates and displays the main application window.
 */
public class TetrisMain {

  /**
   * The title of the main application window.
   */
  public static final String WINDOW_TITLE = "INF101 Tetris by Thomas Barth";


  /**
   * The main method is the entry point for the Tetris game application.
   * It creates and initializes the game board, tetromino factory, model, view, and controller.
   * It also creates and displays the main application window.
   *
   * @param args the command line arguments
   */
  public static void main(String[] args) {
    // Create the game board
    TetrisBoard board = new TetrisBoard(20,10);

    // Create the tetromino factory
    TetrominoFactory factory = new RandomTetrominoFactory();

    // Create the tetris model
    TetrisModel model = new TetrisModel(board,factory);

    // Create the tetris view
    TetrisView view = new TetrisView(model);

    // Create the tetris controller
    TetrisController controller = new TetrisController(model,view);

    // The JFrame is the "root" application window.
    // We here set som properties of the main window, 
    // and tell it to display our tetrisView
    // Create the main application window
    JFrame frame = new JFrame(WINDOW_TITLE);
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.setResizable(false);

    // Here we set which component to view in our window
    frame.setContentPane(view);

    // Call these methods to actually display the window
    frame.pack();
    frame.setVisible(true);
  }
  
}
