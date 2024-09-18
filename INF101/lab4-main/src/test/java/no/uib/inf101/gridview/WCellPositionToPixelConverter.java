package no.uib.inf101.gridview;

//  This class is a "wrapper" around CellPositionToPixelConverter (the
//  class you will write). It uses some concepts that are outside the
//  syllabus of INF101, and you are not expected to understand the code.
//
//  We use this class to be able to write the tests in
//  TestCellPositionToPixelConverter without getting compilation errors,
//  even if CellPositionToPixelConverter is not implemented yet. This is
//  done in the context of a scaffolded homework assignment, and is not
//  something which is normally seen in the wild.

import no.uib.inf101.colorgrid.CellPosition;
import no.uib.inf101.colorgrid.GridDimension;

import java.awt.geom.Rectangle2D;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;

public class WCellPositionToPixelConverter {

  CellPositionToPixelConverter converter;

  public WCellPositionToPixelConverter(Rectangle2D box, GridDimension gd,
                                       double margin) {
    this.converter = newCellPositionToPixelConverter(box, gd, margin);
  }

  public Rectangle2D getBoundsForCell(CellPosition cp) {
    try {
      Method method = CellPositionToPixelConverter.class.getDeclaredMethod(
          "getBoundsForCell", CellPosition.class);
      // Check that the method is public
      assertFalse(Modifier.isPrivate(method.getModifiers()),
          "The method getBoundsForCell(CellPosition) in the " +
              "CellPositionToPixelConverter class should not be private");

      Object result = method.invoke(this.converter, cp);
      assertInstanceOf(Rectangle2D.class, result,
          "The method getBoundsForCell(CellPosition) in the " +
              "CellPositionToPixelConverter class should return a " +
              "Rectangle2D");
      return (Rectangle2D) result;
    } catch (NoSuchMethodException e) {
      fail("Could not find the method getBoundsForCell(CellPosition) in " +
          "the CellPositionToPixelConverter class " + e);
    } catch (IllegalAccessException | InvocationTargetException e) {
      throw new RuntimeException(e);
    }
    throw new IllegalStateException("Should not be possible to reach this " +
        "point");
  }


  static CellPositionToPixelConverter newCellPositionToPixelConverter(
      Rectangle2D box, GridDimension gd, double margin) {
    try {
      Constructor<?> constructor = CellPositionToPixelConverter.class
          .getConstructor(
              Rectangle2D.class, GridDimension.class, double.class);

      // Check that the constructor is public
      assertTrue(Modifier.isPublic(constructor.getModifiers()),
          "The constructor CellPositionToPixelConverter" +
              "(Rectangle2D, GridDimension, double) should be public");

      // Create a new object using the constructor and return it
      return (CellPositionToPixelConverter) constructor.newInstance(box, gd,
          margin);
    } catch (NoSuchMethodException e) {
      fail("Could not find the constructor CellPositionToPixelConverter" +
          "(Rectangle2D, GridDimension, double) in the " +
          "CellPositionToPixelConverter class");
    } catch (InvocationTargetException
             | InstantiationException
             | IllegalAccessException e) {
      throw new RuntimeException(e);
    }
    throw new IllegalStateException("Shouldn't reach this point");
  }
}
