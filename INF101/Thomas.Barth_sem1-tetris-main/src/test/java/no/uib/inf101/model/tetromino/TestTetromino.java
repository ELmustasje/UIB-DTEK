package no.uib.inf101.model.tetromino;

import no.uib.inf101.grid.CellPosition;
import no.uib.inf101.grid.GridCell;
import no.uib.inf101.tetris.model.TetrisBoard;
import no.uib.inf101.tetris.model.tetromino.Tetromino;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.*;

public class TestTetromino {
    @Test
    public void testHashCodeAndEquals() {
        Tetromino t1 = Tetromino.newTetromino('T');
        Tetromino t2 = Tetromino.newTetromino('T');
        Tetromino t3 = Tetromino.newTetromino('T').shiftedBy(1, 0);
        Tetromino s1 = Tetromino.newTetromino('S');
        Tetromino s2 = Tetromino.newTetromino('S').shiftedBy(0, 0);
        assertEquals(t1, t2);
        assertEquals(s1, s2);
        assertEquals(t1.hashCode(), t2.hashCode());
        assertEquals(s1.hashCode(), s2.hashCode());
        assertNotEquals(t1, t3);
        assertNotEquals(t1, s1);
    }

    @Test
    public void tetrominoIterationOfT() {
        // Create a standard 'T' tetromino placed at (10, 100) to test
        Tetromino tetro = Tetromino.newTetromino('T');
        tetro = tetro.shiftedBy(10, 100);

        // Collect which objects are iterated through
        List<GridCell<Character>> objs = new ArrayList<>();
        for (GridCell<Character> gc : tetro) {
            objs.add(gc);
        }

        // Check that we got the expected GridCell objects
        assertEquals(4, objs.size());
        assertTrue(objs.contains(new GridCell<>(new CellPosition(11, 100), 'T')));
        assertTrue(objs.contains(new GridCell<>(new CellPosition(11, 101), 'T')));
        assertTrue(objs.contains(new GridCell<>(new CellPosition(11, 102), 'T')));
        assertTrue(objs.contains(new GridCell<>(new CellPosition(12, 101), 'T')));
    }

    @Test
    public void tetrominoIterationOfS() {
        Tetromino t1 = Tetromino.newTetromino('S');
        t1 = t1.shiftedBy(10, 100);
        List<GridCell<Character>> objs = new ArrayList<>();

        for (GridCell<Character> gc : t1) {
            objs.add(gc);
        }
        assertEquals(4, objs.size());
        assertTrue(objs.contains(new GridCell<>(new CellPosition(11, 101), 'S')));
        assertTrue(objs.contains(new GridCell<>(new CellPosition(11, 102), 'S')));
        assertTrue(objs.contains(new GridCell<>(new CellPosition(12, 100), 'S')));
        assertTrue(objs.contains(new GridCell<>(new CellPosition(12, 101), 'S')));
    }

    @Test
    public void moveTwice() {
        Tetromino t1 = Tetromino.newTetromino('S');
        t1 = t1.shiftedBy(1, 1).shiftedBy(1, 1);
        CellPosition expected = new CellPosition(2, 2);
        assertEquals(expected, t1.pos);
    }

    @Test
    public void shiftedToTopCenterOf() {
        TetrisBoard testBoard1 = new TetrisBoard(9, 9);
        TetrisBoard testBoard2 = new TetrisBoard(10, 10);
        Tetromino t1 = Tetromino.newTetromino('L');
        Tetromino t2 = Tetromino.newTetromino('O');
        t1 = t1.shiftedToTopCenterOf(testBoard1);
        t2 = t2.shiftedToTopCenterOf(testBoard1);
        CellPosition t1ExpectedPos = new CellPosition(-1, 3);
        CellPosition t2ExpectedPos = new CellPosition(-1, 2);
        assertEquals(t1ExpectedPos, t1.pos);
        assertEquals(t2ExpectedPos, t2.pos);

        t1 = t1.shiftedToTopCenterOf(testBoard2);
        t2 = t2.shiftedToTopCenterOf(testBoard2);
        t1ExpectedPos = new CellPosition(-1, 4);
        t2ExpectedPos = new CellPosition(-1, 3);
        assertEquals(t1ExpectedPos, t1.pos);
        assertEquals(t2ExpectedPos, t2.pos);


    }

    @Test
    public void testRotate() {
        Tetromino testTetromino = Tetromino.newTetromino('T');
        Boolean[][] rotatedT = {
                {false, true, false},
                {true, true, false},
                {false, true, false}
        };
        Tetromino oldTetromino = testTetromino;
        Tetromino newTetromino = testTetromino.rotate();
        assertNotEquals(oldTetromino, newTetromino);
        assertEquals(rotatedT, newTetromino.shape);
    }
}
