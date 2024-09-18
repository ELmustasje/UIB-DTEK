package no.uib.inf101.colorgrid;

import java.awt.Color;

public interface IColorGrid extends GridDimension, CellColorCollection {

  /**
   * Get the color of the cell at the given position.
   *
   * @param pos the position
   * @return the color of the cell
   * @throws IndexOutOfBoundsException if the position is out of bounds
   */
  Color get(CellPosition pos);

  /**
   * Set the color of the cell at the given position.
   *
   * @param pos the position
   * @param color the new color
   * @throws IndexOutOfBoundsException if the position is out of bounds
   */
  void set(CellPosition pos, Color color);

}