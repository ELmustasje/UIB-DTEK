package no.uib.inf101.tetris.model.tetromino;

import no.uib.inf101.grid.CellPosition;
import no.uib.inf101.grid.GridCell;
import no.uib.inf101.grid.GridDimension;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Objects;

/**
 * The Tetromino class represents a tetromino shape in the Tetris game.
 * A tetromino is a geometric shape composed of four squares, connected orthogonally.
 * Each tetromino has a symbol, a shape, and a position on the game grid.
 */
public final class Tetromino extends TetrominoTypes implements Iterable<GridCell<Character>> {
    /**
     * The symbol representing the tetromino.
     */
    private char symbol;

    /**
     * The shape of the tetromino.
     * The shape is represented as a 2D boolean array, where true indicates a filled square and false indicates an empty square.
     */
    public boolean[][] shape;

    /**
     * The position of the tetromino on the game grid.
     */
    public CellPosition pos;

    /**
     * Constructs a new Tetromino object with the given symbol, shape, and position.
     *
     * @param symbol The symbol representing the tetromino.
     * @param shape  The shape of the tetromino.
     * @param pos    The position of the tetromino on the game grid.
     */
    private Tetromino(char symbol, boolean[][] shape, CellPosition pos) {
        this.symbol = symbol;
        this.shape = shape;
        this.pos = pos;
    }

    /**
     * Creates a new Tetromino object with the given symbol.
     * The shape and position of the tetromino are determined based on the symbol.
     *
     * @param symbol The symbol representing the tetromino.
     * @return A new Tetromino object with the given symbol.
     */
    public static Tetromino newTetromino(char symbol) {
        return new Tetromino(symbol, getShape(symbol), new CellPosition(0, 0));
    }

    /**
     * Creates a new Tetromino object that is shifted by the given amount in rows and columns.
     *
     * @param detltaRow  The amount to shift the tetromino in rows.
     * @param deltaCol   The amount to shift the tetromino in columns.
     * @return A new Tetromino object that is shifted by the given amount.
     */
    public Tetromino shiftedBy(int detltaRow, int deltaCol) {
        return new Tetromino(this.symbol, this.shape, new CellPosition(pos.row() + detltaRow, pos.col() + deltaCol));

    }


    /**
     * Creates a new Tetromino object that is shifted to the top center of the given grid dimension.
     *
     * @param gridDimension The dimension of the game grid.
     * @return A new Tetromino object that is shifted to the top center of the grid.
     */
    public Tetromino shiftedToTopCenterOf(GridDimension gridDimension) {
        return new Tetromino(this.symbol,this.shape, new CellPosition(-1, gridDimension.cols() / 2 - (shape.length - 2)));
    }

    /**
     * Creates a new Tetromino object that is rotated 90 degrees clockwise.
     *
     * @return A new Tetromino object that is rotated 90 degrees clockwise.
     */
    public Tetromino rotate() {
        boolean[][] newShape = new boolean[shape.length][shape.length];
        for (int r = 0; r < shape.length; r++) {
            for (int c = 0; c < shape.length; c++) {
                newShape[c][shape.length - 1 - r] = shape[r][c];
            }
        }
        return new Tetromino(this.symbol, newShape, this.pos);
    }


    /**
     * Returns an iterator over the grid cells occupied by the tetromino.
     *
     * @return An iterator over the grid cells occupied by the tetromino.
     */
    @Override
    public Iterator<GridCell<Character>> iterator() {
        ArrayList<GridCell<Character>> iteratorList = new ArrayList<>();
        for(int i = 0; i < shape.length;i++){
            for(int j = 0; j <shape[i].length;j++){
                if(shape[i][j]){
                 iteratorList.add(new GridCell<>(new CellPosition(pos.row()+i, pos.col()+j),symbol));
                }
            }
        }
        return iteratorList.iterator();
    }



    /**
    * Returns the hash code value for the tetromino.
     @return The hash code value for the tetromino.
     */
    @Override
    public int hashCode() {
        return Objects.hash(this.symbol, Arrays.deepHashCode(this.shape), this.pos);
    }

    /**
     Compares this tetromino to the specified object.
     The result is true if and only if the argument is not null and is a Tetromino object with the same symbol, shape, and position.
     @param obj The object to compare this tetromino against.
     @return true if the tetrominos are equal, false otherwise.
     */
    @Override
    public boolean equals(Object obj) {
        Tetromino t = (Tetromino) obj;
        return (Arrays.deepEquals(this.shape,t.shape)) && (this.pos.equals(t.pos)) && (this.symbol == t.symbol);
    }

}
