package no.uib.inf101.model;

import no.uib.inf101.grid.CellPosition;
import no.uib.inf101.grid.GridCell;
import no.uib.inf101.model.tetromino.PatternedTetrominoFactory;
import no.uib.inf101.tetris.model.TetrisBoard;
import no.uib.inf101.tetris.model.TetrisModel;
import no.uib.inf101.tetris.model.tetromino.TetrominoFactory;
import no.uib.inf101.tetris.view.ViewableTetrisModel;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

public class TestTetrisModel {
    @Test
    public void initialPositionOfO() {
        TetrisBoard board = new TetrisBoard(20, 10);
        TetrominoFactory factory = new PatternedTetrominoFactory("O");
        ViewableTetrisModel model = new TetrisModel(board, factory);

        List<GridCell<Character>> tetroCells = new ArrayList<>();
        for (GridCell<Character> gc : model.fallingTetromino()) {
            tetroCells.add(gc);
        }

        assertEquals(4, tetroCells.size());
        assertTrue(tetroCells.contains(new GridCell<>(new CellPosition(0, 4), 'O')));
        assertTrue(tetroCells.contains(new GridCell<>(new CellPosition(0, 5), 'O')));
        assertTrue(tetroCells.contains(new GridCell<>(new CellPosition(1, 4), 'O')));
        assertTrue(tetroCells.contains(new GridCell<>(new CellPosition(1, 5), 'O')));
    }

    @Test
    public void initialPositionOfI() {
        TetrisBoard board = new TetrisBoard(20, 10);
        TetrominoFactory factory = new PatternedTetrominoFactory("I");
        ViewableTetrisModel model = new TetrisModel(board, factory);

        List<GridCell<Character>> tetroCells = new ArrayList<>();
        for (GridCell<Character> gc : model.fallingTetromino()) {
            tetroCells.add(gc);
        }
        assertEquals(4, tetroCells.size());
        assertTrue(tetroCells.contains(new GridCell<>(new CellPosition(0, 3), 'I')));
        assertTrue(tetroCells.contains(new GridCell<>(new CellPosition(0, 4), 'I')));
        assertTrue(tetroCells.contains(new GridCell<>(new CellPosition(0, 5), 'I')));
        assertTrue(tetroCells.contains(new GridCell<>(new CellPosition(0, 6), 'I')));
    }

    @Test
    public void SimpleMoveTetromino() {
        TetrisBoard board = new TetrisBoard(20, 10);
        TetrominoFactory factory = new PatternedTetrominoFactory("I");
        TetrisModel model = new TetrisModel(board, factory);

        assertTrue(model.moveTetromino(0, 1));
        assertTrue(model.moveTetromino(1, 0));


    }

    @Test
    public void newIterable() {
        TetrisBoard board = new TetrisBoard(20, 10);
        TetrominoFactory factory = new PatternedTetrominoFactory("I");
        TetrisModel model = new TetrisModel(board, factory);

        Iterable<GridCell<Character>> oldTet = model.fallingTetromino();
        model.moveTetromino(1, 1);
        Iterable<GridCell<Character>> newTet = model.fallingTetromino();
        assertNotEquals(oldTet, newTet);
    }

    @Test
    public void cannotMoveOutsideBounds() {
        TetrisBoard board = new TetrisBoard(20, 10);
        TetrominoFactory factory = new PatternedTetrominoFactory("I");
        TetrisModel model = new TetrisModel(board, factory);

        Iterable<GridCell<Character>> oldTet = model.fallingTetromino();
        assertFalse(model.moveTetromino(30, 0));
        assertFalse(model.moveTetromino(0, 30));
        Iterable<GridCell<Character>> newTet = model.fallingTetromino();
        assertEquals(oldTet, newTet);
    }

    @Test
    public void cannotMoveToOccupied() {
        TetrisBoard board = new TetrisBoard(20, 10);
        TetrominoFactory factory = new PatternedTetrominoFactory("I");
        TetrisModel model = new TetrisModel(board, factory);
        for (int i = 0; i < 10; i++) {
            board.set(new CellPosition(1, i), 'T');
        }
        Iterable<GridCell<Character>> oldTet = model.fallingTetromino();
        assertFalse(model.moveTetromino(1, 0));
        Iterable<GridCell<Character>> newTet = model.fallingTetromino();
        assertEquals(oldTet, newTet);
    }

    @Test
    public void rotateTetromino() {
        TetrisBoard board = new TetrisBoard(20, 10);
        TetrominoFactory factory = new PatternedTetrominoFactory("I");
        TetrisModel model = new TetrisModel(board, factory);

        Iterable<GridCell<Character>> oldTet = model.fallingTetromino();
        assertTrue(model.rotateTetromino());
        Iterable<GridCell<Character>> newTet = model.fallingTetromino();

        List<GridCell<Character>> expectedTet = new ArrayList<GridCell<Character>>();
        expectedTet.add(new GridCell<>(new CellPosition(0, 5), 'I'));
        expectedTet.add(new GridCell<>(new CellPosition(1, 5), 'I'));
        expectedTet.add(new GridCell<>(new CellPosition(2, 5), 'I'));
        expectedTet.add(new GridCell<>(new CellPosition(3, 5), 'I'));

        List<GridCell<Character>> newTetList = new ArrayList<>();
        Iterator newTetIterator = newTet.iterator();
        while (newTetIterator.hasNext()) {
            newTetList.add((GridCell<Character>) newTetIterator.next());
        }
        for (GridCell<Character> cell : newTetList) {
            assertTrue(expectedTet.contains(cell));
        }
        assertNotEquals(oldTet, newTet);
    }

    @Test
    public void testDrop() {
        TetrisBoard board = new TetrisBoard(20, 10);
        TetrominoFactory factory = new PatternedTetrominoFactory("I");
        TetrisModel model = new TetrisModel(board, factory);

        model.dropTetromino();
        Iterator it = model.getTilesOnBoard().iterator();
        ArrayList<GridCell<Character>> boardList = new ArrayList<>();
        ArrayList<GridCell<Character>> expectedList = new ArrayList<>();
        expectedList.add(new GridCell<>(new CellPosition(19, 3), 'I'));
        expectedList.add(new GridCell<>(new CellPosition(19, 4), 'I'));
        expectedList.add(new GridCell<>(new CellPosition(19, 5), 'I'));
        expectedList.add(new GridCell<>(new CellPosition(19, 6), 'I'));
        while (it.hasNext()) {
            boardList.add((GridCell<Character>) it.next());
        }
        for (GridCell<Character> cell : expectedList) {
            assertTrue(boardList.contains(cell));
        }

        board = new TetrisBoard(20, 10);
        factory = new PatternedTetrominoFactory("I");
        model = new TetrisModel(board, factory);

        model.board.set(new CellPosition(10, 5), 'O');
        model.dropTetromino();

        it = model.getTilesOnBoard().iterator();
        boardList = new ArrayList<>();
        expectedList = new ArrayList<>();
        expectedList.add(new GridCell<>(new CellPosition(9, 3), 'I'));
        expectedList.add(new GridCell<>(new CellPosition(9, 4), 'I'));
        expectedList.add(new GridCell<>(new CellPosition(9, 5), 'I'));
        expectedList.add(new GridCell<>(new CellPosition(9, 6), 'I'));
        while (it.hasNext()) {
            boardList.add((GridCell<Character>) it.next());
        }
        for (GridCell<Character> cell : expectedList) {
            assertTrue(boardList.contains(cell));
        }
    }

    @Test
    public void testClockTick() {
        TetrisBoard board = new TetrisBoard(20, 10);
        TetrominoFactory factory = new PatternedTetrominoFactory("O");
        TetrisModel model = new TetrisModel(board, factory);

        CellPosition oldPos = model.fallingTetromino().iterator().next().pos();
        model.clockTick();
        CellPosition newPos = model.fallingTetromino().iterator().next().pos();
        System.out.println(newPos);
        assertEquals(newPos, new CellPosition(oldPos.row() + 1, oldPos.col()));

        oldPos = newPos;
        board.set(new CellPosition(newPos.row() + 2, newPos.col()), 'O');
        model.clockTick();
        newPos = model.fallingTetromino().iterator().next().pos();
        assertEquals(oldPos, newPos);
    }
}
