package datastructure;

/**
 * A grid dimension is a pair of integers representing the number of
 * rows and columns in a grid.
 */
public interface GridDimension {

  /** Number of rows in the grid. */
  int rows();

  /** Number of columns in the grid. */
  int cols();

  /**
   * Checks if a given CellPosition is within the bounds of the grid.
   *
   * @param pos the position to check
   * @return true if pos is within bounds of the grid, false otherwise.
   */
  default boolean isOnGrid(CellPosition pos) {
    return pos.row() >= 0 && pos.row() < this.rows()
        && pos.col() >= 0 && pos.col() < this.cols();
  }

  /**
   * Makes it possible to iterate over all coordinates of this grid
   * Iteration happens row-wise i.e. First row 0, then row 1 and so on.
   */
  default Iterable<CellPosition> cellPositions() {
    return () -> new CellPositionIterator(this.rows(), this.cols());
  }
}
