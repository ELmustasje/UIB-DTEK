package no.uib.inf101.grid;

import java.util.*;
/**
 * A generic grid implementation that stores elements of type E in a two-dimensional grid.
 */
public class Grid<E> implements IGrid{
    /**
     * The number of rows in the grid.
     */
    private int rows;
    /**
     * The number of columns in the grid.
     */
    private int cols;
    /**
     * The default value to be used for cells in the grid.
     */
    private E defaultValue = null;
    /**
     * A list that stores all the cells in the grid.
     */
    protected List<GridCell<Object>> fullgrid = new LinkedList<>();

    /**
     * Constructs a new Grid object with the specified number of rows and columns, and the given default value.
     *
     * @param rows           the number of rows in the grid
     * @param cols           the number of columns in the grid
     * @param defaultValueV  the default value to be used for cells in the grid
     */

    //metod overload 1
    protected Grid(int rows,int cols,E defaultValueV){
        this.rows = rows;
        this.cols = cols;
        this.defaultValue = defaultValueV;
        createGrid(rows,cols);
    }
    /**
     * Constructs a new Grid object with the specified number of rows and columns, and a null default value.
     *
     * @param rows  the number of rows in the grid
     * @param cols  the number of columns in the grid
     */
    //metod overload 2
    protected Grid(int rows,int cols){
        this.rows = rows;
        this.cols = cols;
        createGrid(rows,cols);
    }
    /**
     * Returns the default value used for cells in the grid.
     *
     * @return the default value used for cells in the grid
     */
    public E getDefaultValue(){
        return defaultValue;
    }

    /**
     * Helper method to create the grid by adding cells to the fullgrid list.
     *
     * @param rows  the number of rows in the grid
     * @param cols  the number of columns in the grid
     */
    private void createGrid(int rows,int cols){
        for(int i = 0;i<rows;i++){
            for(int j = 0;j<cols;j++){
                CellPosition newCellPos = new CellPosition(i,j);
                GridCell<Object> newCell = new GridCell<>(newCellPos,defaultValue);
                fullgrid.add(newCell);
            }
        }
    }
    /**
     * Sets the value of the cell at the specified position.
     *
     * @param pos    the position of the cell
     * @param value  the value to be set
     */
    @Override
    public void set(CellPosition pos, Object value) {
        int index =0;
        for(GridCell cell:fullgrid){
            if (cell.pos().equals(pos)){
                GridCell<Object> newCell = new GridCell<>(pos,value);
                fullgrid.set(index,newCell);
                break;
            }
            index++;
        }
    }
    /**
     * Returns the cell at the specified position.
     *
     * @param pos  the position of the cell
     * @return the cell at the specified position
     * @throws IndexOutOfBoundsException if the position is not in the grid
     */
    @Override
    public GridCell get(CellPosition pos) {
        for(GridCell cell:fullgrid){
            if (cell.pos().equals(pos)){
                return cell;
            }
        }
        throw new IndexOutOfBoundsException("CellPosition not in grid");
    }
    /**
     * Checks if the specified position is within the grid.
     *
     * @param pos  the position to check
     * @return true if the position is within the grid, false otherwise
     */
    @Override
    public boolean positionIsOnGrid(CellPosition pos) {
        for(GridCell cell:fullgrid){
            if (cell.pos().equals(pos)){
                return true;
            }
        }
        return false;
    }
    /**
     * Returns a list of all the cells in the grid.
     *
     * @return a list of all the cells in the grid
     */
    public List<GridCell<Object>> getGridList(){
        return fullgrid;
    }
    /**
     * Returns the number of rows in the grid.
     *
     * @return the number of rows in the grid
     */
    @Override
    public int rows() {
        return rows;
    }
    /**
     * Returns the number of columns in the grid.
     *
     * @return the number of columns in the grid
     */
    @Override
    public int cols() {
        return cols;
    }

    /**
     * Returns an iterator over the cells in the grid. *
     * @return an iterator over the cells in the grid
     */
    @Override
    public Iterator<GridCell<Object>> iterator() {
        return fullgrid.iterator();
    }

}
