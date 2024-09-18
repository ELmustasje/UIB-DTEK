package no.uib.inf101.gridview;

import no.uib.inf101.colorgrid.CellPosition;
import no.uib.inf101.colorgrid.GridDimension;

import java.awt.geom.Rectangle2D;

public class CellPositionToPixelConverter {

    private static Rectangle2D box;
    private static GridDimension gd;
    private static Double margin;

    public CellPositionToPixelConverter(Rectangle2D box, GridDimension gd, double margin){
        this.box = box;
        this.gd = gd;
        this.margin = margin;

    }

    public Rectangle2D getBoundsForCell(CellPosition cp){

        double cellWidth = (box.getWidth() - ((gd.cols() + 1) * margin)) / gd.cols();
        double cellHeight = (box.getHeight() -((gd.rows() + 1) * margin)) / gd.rows();

        double xStep = margin + cellWidth;
        double yStep = margin + cellHeight;

        double cellX = box.getX() + (xStep * cp.col()) + margin;
        double cellY = box.getY() + (yStep * cp.row()) + +margin;

        Rectangle2D cell = new Rectangle2D.Double(cellX,cellY,cellWidth,cellHeight);

        return cell;
    }
  // TODO: Implement this class
}
