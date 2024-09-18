package no.uib.inf101.colorgrid;

//  This class is a "wrapper" around ColorGrid (the class you will
//  write). It uses some concepts that are outside the syllabus of
//  INF101, and you are not expected to understand the code in the
//  createColorGrid method.
//
//  We use this class to be able to write the tests in TestColorGrid
//  without getting compilation errors, even if ColorGrid is not
//  implemented yet. This is done in the context of a scaffolded
//  homework assignment, and is not something which is normally seen in
//  the wild.

import java.awt.Color;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.List;

import static org.junit.jupiter.api.Assertions.fail;

public class WColorGrid implements IColorGrid {

  private final IColorGrid cg;

  public WColorGrid(int rows, int cols) {
    this.cg = newColorGrid(rows, cols);
  }

  @Override
  public List<CellColor> getCells() {
    return this.cg.getCells();
  }

  @Override
  public int rows() {
    return this.cg.rows();
  }

  @Override
  public int cols() {
    return this.cg.cols();
  }

  @Override
  public Color get(CellPosition pos) {
    return this.cg.get(pos);
  }

  @Override
  public void set(CellPosition pos, Color color) {
    this.cg.set(pos, color);
  }

  /**
   * Create a new ColorGrid with the given rows and cols. This method
   * will only work if you have implemented the ColorGrid class with
   * the correct parameters (two int's), otherwise the test will fail
   * when calling this method.
   *
   * @param rows  number of rows in the colorgrid to create
   * @param cols  number of columns in the colorgrid to create
   * @return    a new ColorGrid
   */
  private static IColorGrid newColorGrid(int rows, int cols) {
    try {
      Constructor<?> c = ColorGrid.class.getConstructor(int.class, int.class);
      Object o = c.newInstance(rows, cols);
      if (o instanceof IColorGrid grid) {
        return grid;
      }
      fail("ColorGrid constructor did not return an IColorGrid. This could "
          + "be because you forgot to implement the IColorGrid interface.");
    } catch (NoSuchMethodException e) {
      fail("Could not find constructor ColorGrid(int, int)");
    } catch (InvocationTargetException e) {
      fail("Constructor crashed: " + e);
    } catch (InstantiationException e) {
      throw new RuntimeException(e);
    } catch (IllegalAccessException e) {
      fail("Constructor is not public: " + e);
    }
    return null;
  }

  public static IColorGrid newGridFromStrings(String[] sgrid) {
    IColorGrid grid = new WColorGrid(sgrid.length, sgrid[0].length());
    for (int r = 0; r < grid.rows(); r++) {
      for (int c = 0; c < grid.cols(); c++) {
        grid.set(new CellPosition(r, c), switch (sgrid[r].charAt(c)) {
          case 'R' -> Color.RED;
          case 'G' -> Color.GREEN;
          case 'B' -> Color.BLUE;
          case 'Y' -> Color.YELLOW;
          default -> null;
        });
      }
    }
    return grid;
  }

}
