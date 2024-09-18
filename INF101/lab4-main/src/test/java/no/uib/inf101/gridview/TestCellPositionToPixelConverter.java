package no.uib.inf101.gridview;

import no.uib.inf101.colorgrid.*;
import org.junit.jupiter.api.Test;

import java.awt.*;
import java.awt.geom.Rectangle2D;

import static org.junit.jupiter.api.Assertions.*;

public class TestCellPositionToPixelConverter {

  @Test
  public void sanityTest() {
    // Same case as illustrated in README
    // Setup
    IColorGrid grid = new WColorGrid(3, 4);
    grid.set(new CellPosition(0, 0), Color.RED);
    grid.set(new CellPosition(0, 3), Color.BLUE);
    grid.set(new CellPosition(2, 0), Color.YELLOW);
    grid.set(new CellPosition(2, 3), Color.GREEN);

    // Construct CellPositionToPixelConverter (using a wrapper class)
    WCellPositionToPixelConverter converter = new WCellPositionToPixelConverter(
        new Rectangle2D.Double(30, 30, 340, 240), grid,30
    );

    // Check result
    Rectangle2D expected = new Rectangle2D.Double(215, 130, 47.5, 40);
    assertEquals(expected, converter.getBoundsForCell(new CellPosition(1, 2)));
  }
}
