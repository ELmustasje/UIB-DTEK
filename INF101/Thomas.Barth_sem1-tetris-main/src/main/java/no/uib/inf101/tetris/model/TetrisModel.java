package no.uib.inf101.tetris.model;

import no.uib.inf101.grid.GridCell;
import no.uib.inf101.grid.GridDimension;
import no.uib.inf101.tetris.controller.ControllableTetrisModel;
import no.uib.inf101.tetris.model.tetromino.Tetromino;
import no.uib.inf101.tetris.model.tetromino.TetrominoFactory;
import no.uib.inf101.tetris.view.ViewableTetrisModel;

/**
 * The TetrisModel class represents the model of the Tetris game.
 * It implements both the ViewableTetrisModel and ControllableTetrisModel interfaces.
 * It contains methods for manipulating the game state, moving and rotating the tetromino,
 * and updating the board.
 */
public class TetrisModel implements ViewableTetrisModel, ControllableTetrisModel {

    /**
     * The current game state.
     */
    private GameState gameState = GameState.ACTIVE_GAME;

    /**
     * The TetrisBoard object representing the game board.
     */
    public TetrisBoard board;

    /**
     * The TetrominoFactory object used to generate new tetrominos.
     */
    private TetrominoFactory factory;

    /**
     * The current tetromino in play.
     */
    private Tetromino tetromino;


    /**
     * Constructs a new TetrisModel with the given TetrisBoard and TetrominoFactory.
     *
     * @param board   The TetrisBoard object representing the game board.
     * @param factory The TetrominoFactory object used to generate new tetrominos.
     */
    public TetrisModel(TetrisBoard board, TetrominoFactory factory){
        this.board = board;
        this.factory = factory;
        this.tetromino = factory.getNext().shiftedToTopCenterOf(board);
    }

    /**
     * Generates a new tetromino and sets it as the current tetromino.
     * If the new tetromino cannot be placed on the board, the game state is set to GAME_OVER.
     */
    private void getNewTetromino(){
        Tetromino newTet = factory.getNext().shiftedToTopCenterOf(board);
        if(!canMove(newTet)){
            gameState = GameState.GAME_OVER;
        }else {
            this.tetromino = newTet;
        }
    }

    /**
     * Sets the current tetromino on the board and removes any full rows.
     * Generates a new tetromino afterward.
     */
    private void setTetrominoToBoard(){
        for (GridCell<Character> tet : fallingTetromino()) {
            board.set(tet.pos(), tet.value());
        }
        board.removeFullRows();
        getNewTetromino();
    }

    /**
     * Returns the number of rows in the game board.
     *
     * @return The number of rows in the game board.
     */
    @Override
    public int rows() {
        return board.rows();
    }

    /**
     * Returns the number of columns in the game board.
     *
     * @return The number of columns in the game board.
     */
    @Override
    public int cols() {
        return board.cols();
    }

    /**
     * Returns the dimensions of the game board.
     *
     * @return The dimensions of the game board.
     */
    @Override
    public GridDimension getDimention() {
        return new GridDimension() {
            @Override
            public int rows() {
                return board.rows();
            }

            @Override
            public int cols() {
                return board.cols();
            }
        };
    }

    /**
     * Returns an Iterable of GridCells representing the tiles on the game board.
     *
     * @return An Iterable of GridCells representing the tiles on the game board.
     */
    @Override
    public Iterable<GridCell<Character>> getTilesOnBoard() {
        return this.board;
    }

    /**
     * Returns an Iterable of GridCells representing the current falling tetromino.
     *
     * @return An Iterable of GridCells representing the current falling tetromino.
     */
    @Override
    public Iterable<GridCell<Character>> fallingTetromino() {
        return this.tetromino;
    }

    /**
     * Returns the current game state.
     *
     * @return The current game state.
     */
    @Override
    public GameState getGameState() {
        return this.gameState;
    }

    /**
     * Returns the time interval in milliseconds between clock ticks.
     *
     * @return The time interval in milliseconds between clock ticks.
     */
    @Override
    public int getMillis() {
        return 1000;
    }

    /**
     * Advances the game by one clock tick.
     * If the current tetromino cannot move down, it is set on the board and a new tetromino is generated.
     */
    @Override
    public void clockTick() {
        if(!moveTetromino(1,0)) {
            setTetrominoToBoard();
        }
    }


    /**
     * Moves the current Tetromino by the specified delta in rows and columns.
     *
     * @param deltaRow the number of rows to move the Tetromino
     * @param deltaCol the number of columns to move the Tetromino
     * @return true if the Tetromino was successfully moved, false otherwise
     */
    @Override
    public boolean moveTetromino(int deltaRow, int deltaCol) {
        Tetromino testTetromino = tetromino.shiftedBy(deltaRow, deltaCol);
        if (canMove(testTetromino)) {
            this.tetromino = testTetromino;
            return true;
        }
        return false;
    }

    /**
     * Rotates the current Tetromino. If the rotation is successful,
     * the Tetromino is updated. If not, the Tetromino is kicked back
     * to a valid position if possible.
     *
     * @return true if the Tetromino was successfully rotated or kicked, false otherwise
     */
    @Override
    public boolean rotateTetromino() {
        Tetromino testTetromino = tetromino.rotate();
        if(canMove(testTetromino)){
            this.tetromino = testTetromino;
            return true;
        }
        //kickback
        if(tetromino.pos.col() < 0 || (tetromino.pos.col() + tetromino.shape.length) > board.cols()){
            for(int i = 1; i <= tetromino.shape.length/2; i++){
                Tetromino kickTetrominoRigth = tetromino.shiftedBy(0,i).rotate();
                Tetromino kickTetrominoLeft = tetromino.shiftedBy(0,-i).rotate();
                if(canMove(kickTetrominoRigth)){
                    this.tetromino = kickTetrominoRigth;
                    return true;
                } else if (canMove(kickTetrominoLeft)) {
                    this.tetromino = kickTetrominoLeft;
                    return true;

                }
            }
        }
        if(tetromino.pos.row() < 0){
            for(int i = 1; i <= 2; i++){
                Tetromino kickTetrominoUp = tetromino.shiftedBy(i,0).rotate();
                if(canMove(kickTetrominoUp)){
                    this.tetromino = kickTetrominoUp;
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Drops the current Tetromino until it cannot move further.
     * The Tetromino is then placed onto the game board.
     */
    @Override
    public void dropTetromino() {
        int i = 1;
        Tetromino dropTet = tetromino;
        while (canMove(dropTet.shiftedBy(i,0))){
            dropTet = dropTet.shiftedBy(i,0);
        }
        tetromino = dropTet;
        setTetrominoToBoard();
    }

    /**
     * Checks if the specified Tetromino can be moved to its new position
     * without overlapping with existing Tetrominos on the game board.
     *
     * @param tet the Tetromino to check for movement validity
     * @return true if the Tetromino can move, false otherwise
     */
    private boolean canMove(Tetromino tet){
        for(GridCell<Character> gc : tet){
            try {
                GridCell cell = board.get(gc.pos());
                if(!cell.value().equals('-')){
                    return false;
                }
            }catch (IndexOutOfBoundsException e){
                return false;
            }

        }
        return true;
    }
}
