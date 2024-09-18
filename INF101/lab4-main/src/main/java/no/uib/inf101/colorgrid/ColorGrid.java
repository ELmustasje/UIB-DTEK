package no.uib.inf101.colorgrid;

import java.awt.*;
import java.util.ArrayList;
import java.util.List;


public class ColorGrid implements IColorGrid {
    public static void main(String[] args) {
        ColorGrid test = new ColorGrid(10,20);
        test.set(new CellPosition(0,0),Color.red);
    }
    private int rows;
    private int cols;
    List<CellColor> grid;

    public ColorGrid(int rows,int cols){
        this.rows = rows;
        this.cols = cols;
        int cap = rows * cols;
        grid = new ArrayList<>(cap);
        for(int r = 0; r < rows; r++){
            for(int c = 0; c < cols; c++){
                CellColor cell = new CellColor(new CellPosition(r,c),null);
                grid.add(cell);
            }
        }
    }
    @Override
    public List<CellColor> getCells() {
        return grid;
    }

    @Override
    public int rows() {
        return this.rows;
    }

    @Override
    public int cols() {
        return this.cols;
    }

    @Override
    public Color get(CellPosition pos) {
        if(pos.row() < 0 || pos.row() >= rows || pos.col() < 0 || pos.col() >= cols){
            throw new IndexOutOfBoundsException("index out of range");
        }
        for (CellColor cell : grid){
            if (cell.cellPosition().equals(pos)){
                return cell.color();
            }
        }
        return null;
    }

    @Override
    public void set(CellPosition pos, Color color) {
        if(pos.row() < 0 || pos.row() >= rows || pos.col() < 0 || pos.col() >= cols){
            throw new IndexOutOfBoundsException("index out of range");
        }
        CellColor newCell = new CellColor(pos,color);
        int index = 0;
        for(CellColor cell : grid){
            if(cell.cellPosition().equals(pos)){
                grid.set(index,newCell);
                break;
            }
            index++;
        }
    }


    // TODO: Implement this class

}
