package no.uib.inf101.gridview;

// NOTE: This file does NOT demonstrate a good way to write a test!
// Most tests in this file are written in a way that makes it possible
// to test HOW the problem is solved instead of testing simply THAT
// the problem is solved. This is done to be able to test that the
// detailed instructions for the assignment are followed, and could
// provide hints in an educational setting in case something is off.
//
// In a real project, you should not test private methods, but instead
// test public or package-private methods. Test *what* the code does,
// not *how* it does it (like we do here).

import no.uib.inf101.colorgrid.*;
import org.junit.jupiter.api.Test;

import java.awt.*;
import java.awt.geom.Rectangle2D;

import static org.junit.jupiter.api.Assertions.*;

public class TestGridView {

  @Test
  public void preferredSizeTest() {
    WGridView view = new WGridView(null);
    assertNotNull(view.getPreferredSize(), "PreferredSize shouldn't be null");
    assertEquals(400, view.getPreferredSize().width);
    assertEquals(300, view.getPreferredSize().height);
  }

  @Test
  public void drawCellsSanityTest() {
    // Sample case illustrated in README
    IColorGrid grid = WColorGrid.newGridFromStrings(new String[]{
        "R--B",
        "----",
        "Y--G"
    });
    Rectangle2D box = new Rectangle2D.Double(30, 30, 340, 240);
    double margin = 30;
    RecordGraphics2D recording = singleDrawCellsRun(grid, box, margin);

    // Check that the correct number of calls were filled
    assertEquals(12, recording.getRecordedFillShapes().size(),
        "The drawCells method call draw 12 rectangles in the illustrated sample case");

    assertColorIsDrawnOnceAt(recording, Color.RED, new Rectangle2D.Double(60, 60, 47.5, 40));
    assertColorIsDrawnOnceAt(recording, Color.BLUE, new Rectangle2D.Double(292.5, 60, 47.5, 40));
    assertColorIsDrawnOnceAt(recording, Color.YELLOW, new Rectangle2D.Double(60, 200, 47.5, 40));
    assertColorIsDrawnOnceAt(recording, Color.GREEN, new Rectangle2D.Double(292.5, 200, 47.5, 40));
  }

  @Test
  public void drawCellsSanityOtherColors() {
    // Sample case illustrated in README, but colors mixed up
    IColorGrid sampleGrid = WColorGrid.newGridFromStrings(new String[]{
        "Y--R",
        "----",
        "G--B"
    });
    Rectangle2D rect = new Rectangle2D.Double(30, 30, 340, 240);
    double margin = 30;
    RecordGraphics2D record = singleDrawCellsRun(sampleGrid, rect, margin);

    // Check that the correct number of calls were made
    assertEquals(12, record.getRecordedFillShapes().size());

    assertColorIsDrawnOnceAt(record, Color.YELLOW, new Rectangle2D.Double(60, 60, 47.5, 40));
    assertColorIsDrawnOnceAt(record, Color.RED, new Rectangle2D.Double(292.5, 60, 47.5, 40));
    assertColorIsDrawnOnceAt(record, Color.GREEN, new Rectangle2D.Double(60, 200, 47.5, 40));
    assertColorIsDrawnOnceAt(record, Color.BLUE, new Rectangle2D.Double(292.5, 200, 47.5, 40));
  }

  @Test
  public void drawCellsSanityTestPixelsMoved() {
    // Sample case from assignment text
    IColorGrid sampleGrid = WColorGrid.newGridFromStrings(new String[]{
        "R--B",
        "----",
        "Y--G"
    });
    Rectangle2D rect = new Rectangle2D.Double(40, 20, 342, 243);
    double margin = 30;
    RecordGraphics2D record = singleDrawCellsRun(sampleGrid, rect, margin);

    // Check that the correct number of calls were made
    assertEquals(12, record.getRecordedFillShapes().size());

    assertColorIsDrawnOnceAt(record, Color.RED, new Rectangle2D.Double(70, 50, 48, 41));
    assertColorIsDrawnOnceAt(record, Color.BLUE, new Rectangle2D.Double(304, 50, 48, 41));
    assertColorIsDrawnOnceAt(record, Color.YELLOW, new Rectangle2D.Double(70, 192, 48, 41));
    assertColorIsDrawnOnceAt(record, Color.GREEN, new Rectangle2D.Double(304, 192, 48, 41));
  }

  @Test
  public void drawCellsSingleCell() {
    // Sample case from assignment text
    IColorGrid sampleGrid = WColorGrid.newGridFromStrings(new String[] {"R"});
    Rectangle2D rect = new Rectangle2D.Double(20, 30, 40, 50);
    double margin = 10;
    RecordGraphics2D record = singleDrawCellsRun(sampleGrid, rect, margin);

    // Check that the correct number of calls were made
    assertEquals(1, record.getRecordedFillShapes().size());
    assertColorIsDrawnOnceAt(record, Color.RED, new Rectangle2D.Double(30, 40, 20, 30));
  }

  /////////////////////////////
  // Helper methods
  /////////////////////////////

  private void assertColorIsDrawnOnceAt(RecordGraphics2D record, Color color, Rectangle2D expectedRectangle) {
    int count = 0;
    for (int i = 0; i<record.getRecordedFillColors().size(); i++) {
      if (record.getRecordedFillColors().get(i).equals(color)) {
        assertEquals(expectedRectangle, record.getRecordedFillShapes().get(i),
            "Incorrect bounds for color "+ color
        );
        count++;
      }
    }
    assertEquals(1, count, "There should be exactly one rectangle with color " + color);
  }

  public static RecordGraphics2D singleDrawCellsRun(
      IColorGrid grid, Rectangle2D rect, double margin) {
    // Get the "drawCells" method from the GridView class and invoke
    // the method with the fake Graphics2D object
    RecordGraphics2D g2 = new RecordGraphics2D();
    CellPositionToPixelConverter converter = WCellPositionToPixelConverter.newCellPositionToPixelConverter(
        rect, grid, margin);
    WGridView.drawCells(g2, grid, converter);
    return g2;
  }
}
