package no.uib.inf101.gridview;

//  This class is a "wrapper" around GridView (the class you will
//  write). It uses some concepts that are outside the syllabus of
//  INF101, and you are not expected to understand the code.
//
//  We use this class to be able to write the tests in TestGridView
//  without getting compilation errors, even if GridView is not
//  implemented yet. This is done in the context of a scaffolded
//  homework assignment, and is not something which is normally seen in
//  the wild.

import no.uib.inf101.colorgrid.CellColorCollection;
import no.uib.inf101.colorgrid.IColorGrid;

import javax.swing.JPanel;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import static org.junit.jupiter.api.Assertions.*;

public class WGridView extends JPanel {

  private final GridView gv;
  private final JPanel jp;

  public WGridView(IColorGrid grid) {
    GridView gv = WGridView.newGridView(grid);
    assertInstanceOf(JPanel.class, gv, "GridView objects does not " +
        "carry the type JPanel. Does your GridView class extend JPanel?");
    this.gv = gv;
    if ((Object) gv instanceof JPanel jp) {
      this.jp = jp;
    } else {
      throw new IllegalStateException("Should not be here");
    }
  }

  @Override
  public Dimension getPreferredSize() {
    return this.jp.getPreferredSize();
  }

  static void drawCells(Graphics2D g2, CellColorCollection cells,
                        CellPositionToPixelConverter converter) {
    try {
      Method drawCell = GridView.class.getDeclaredMethod("drawCells",
          Graphics2D.class,
          CellColorCollection.class,
          CellPositionToPixelConverter.class
      );

      // Test that the method is static
      assertTrue(Modifier.isStatic(drawCell.getModifiers()),
          "The drawCells method should be static, " +
              "and it should not use any instance variables");

      // Test that the method is private
      assertTrue(Modifier.isPrivate(drawCell.getModifiers()),
          "The drawCells method should be private");

      // Make the method accessible in case it is private
      drawCell.setAccessible(true);

      // Invoke the method
      drawCell.invoke(null, g2, cells, converter);
    } catch (NoSuchMethodException e) {
      fail("Could not find the method drawCells(Graphics2D, " +
          "CellColorCollection, CellPositionToPixelConverter) in the " +
          "GridView class");
    } catch (InvocationTargetException | IllegalAccessException e) {
      throw new RuntimeException(e);
    }
  }



  private static GridView newGridView(IColorGrid grid) {
    try {
      Constructor<?> constructor = GridView.class.getConstructor(
          IColorGrid.class);

      // Check that the constructor is not private
      assertFalse(Modifier.isPrivate(constructor.getModifiers()),
          "The constructor GridView(IColorGrid) should not " +
              "be private");

      // Create a new object using the constructor and return it
      return (GridView) constructor.newInstance(grid);
    } catch (NoSuchMethodException e) {
      if (grid != null) {
        fail("Could not find the constructor GridView(IColorGrid) in " +
            "the GridView class");
      }
      try {
        Constructor<?> constructor = GridView.class.getConstructor();
        return (GridView) constructor.newInstance();
      } catch (NoSuchMethodException ex) {
        fail("Could not find the constructor GridView() or " +
            "GridView(IColorGrid) in the GridView class");
      } catch (InvocationTargetException
               | InstantiationException
               | IllegalAccessException ex) {
        throw new RuntimeException(ex);
      }
    } catch (InvocationTargetException
             | InstantiationException
             | IllegalAccessException e) {
      throw new RuntimeException(e);
    }
    throw new IllegalStateException("Shouldn't reach this point");
  }
}
