package no.uib.inf101.grid;

import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Testing the class Grid
 */
public class GridTest {

    @Test
    void gridTestGetRowsAndCols() {
        IGrid<Integer> grid = new Grid<>(3, 2);
        assertEquals(3, grid.rows());
        assertEquals(2, grid.cols());
    }

    @Test
    void gridSanityTest() {
        String defaultValue = "x";
        IGrid<GridCell> grid = new Grid<>(3, 2, defaultValue);

        assertEquals(3, grid.rows());
        assertEquals(2, grid.cols());

        assertEquals("x", grid.get(new CellPosition(0, 0)).value());
        assertEquals("x", grid.get(new CellPosition(2, 1)).value());

        grid.set(new CellPosition(1, 1), "y");

        assertEquals("y", grid.get(new CellPosition(1, 1)).value());
        assertEquals("x", grid.get(new CellPosition(0, 1)).value());
        assertEquals("x", grid.get(new CellPosition(1, 0)).value());
        assertEquals("x", grid.get(new CellPosition(2, 1)).value());
    }

    @Test
    void gridCanHoldNull() {
        String defaultValue = "x";
        IGrid<GridCell> grid = new Grid<>(3, 2, defaultValue);
        assertEquals("x", grid.get(new CellPosition(0, 0)).value());
        assertEquals("x", grid.get(new CellPosition(2, 1)).value());

        grid.set(new CellPosition(1, 1), null);

        assertEquals(null, grid.get(new CellPosition(1, 1)).value());
        assertEquals("x", grid.get(new CellPosition(0, 1)).value());
        assertEquals("x", grid.get(new CellPosition(1, 0)).value());
        assertEquals("x", grid.get(new CellPosition(2, 1)).value());
    }

    @Test
    void gridNullsInDefaultConstructor() {
        IGrid<GridCell> grid = new Grid<>(3, 2);

        assertEquals(null, grid.get(new CellPosition(0, 0)).value());
        assertEquals(null, grid.get(new CellPosition(2, 1)).value());

        grid.set(new CellPosition(1, 1), 'y');

        assertEquals('y', grid.get(new CellPosition(1, 1)).value());
        assertEquals(null, grid.get(new CellPosition(0, 1)).value());
        assertEquals(null, grid.get(new CellPosition(1, 0)).value());
        assertEquals(null, grid.get(new CellPosition(2, 1)).value());
    }

    @Test
    void coordinateIsOnGridTest() {
        IGrid<Double> grid = new Grid<>(3, 2, 0.9);

        assertTrue(grid.positionIsOnGrid(new CellPosition(2, 1)));
        assertFalse(grid.positionIsOnGrid(new CellPosition(3, 1)));
        assertFalse(grid.positionIsOnGrid(new CellPosition(2, 2)));

        assertTrue(grid.positionIsOnGrid(new CellPosition(0, 0)));
        assertFalse(grid.positionIsOnGrid(new CellPosition(-1, 0)));
        assertFalse(grid.positionIsOnGrid(new CellPosition(0, -1)));
    }

    @Test
    void throwsExceptionWhenCoordinateOffGrid() {
        IGrid<String> grid = new Grid<>(3, 2, "x");

        try {
            @SuppressWarnings("unused")
            String x = grid.get(new CellPosition(3, 1));
            fail();
        } catch (IndexOutOfBoundsException e) {
            // Test passed
        }
    }

    @Test
    void testIterator() {
        IGrid<String> grid = new Grid<>(3, 2, "x");
        grid.set(new CellPosition(0, 0), "a");
        grid.set(new CellPosition(1, 1), "b");
        grid.set(new CellPosition(2, 1), "c");

        List<GridCell<String>> items = new ArrayList<>();
        for (GridCell<String> coordinateItem : grid) {
            items.add(coordinateItem);
        }

        assertEquals(3 * 2, items.size());
        assertTrue(items.contains(new GridCell<String>(new CellPosition(0, 0), "a")));
        assertTrue(items.contains(new GridCell<String>(new CellPosition(1, 1), "b")));
        assertTrue(items.contains(new GridCell<String>(new CellPosition(2, 1), "c")));
        assertTrue(items.contains(new GridCell<String>(new CellPosition(0, 1), "x")));
    }
}
