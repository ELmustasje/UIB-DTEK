package no.uib.inf101.tetris.view;

import no.uib.inf101.grid.CellPosition;
import no.uib.inf101.grid.GridDimension;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.awt.image.ImageObserver;
import java.awt.image.ImageProducer;
import java.io.File;
import java.io.IOException;

/**
 * Converts a cell position to pixel coordinates within a given box with margins.
 */
public class CellPositionToPixelConverter {


    private final Rectangle2D box;
    private final GridDimension gd;
    private final double margin;

    /**
     * Constructs a CellPositionToPixelConverter with the specified box,
     * grid dimension, and margin.
     *
     * @param box the box to convert the cell position to pixel coordinates within
     * @param gd the grid dimension of the cell position
     * @param margin the margin between cells
     */
    public CellPositionToPixelConverter(Rectangle2D box,GridDimension gd, Double margin){
        this.box = box;
        this.gd = gd;
        this.margin = margin;
    }

    /**
     * Returns the pixel bounds for the specified cell position.
     *
     * @param pos the cell position to convert to pixel bounds
     * @return the pixel bounds of the cell position
     */
    protected Rectangle2D getBoundsForCell(CellPosition pos){
        double cellWidth = (box.getWidth() - ((gd.cols() + 1) * margin)) / gd.cols();
        double cellHeight = (box.getHeight() -((gd.rows() + 1) * margin)) / gd.rows();

        double xStep = margin + cellWidth;
        double yStep = margin + cellHeight;

        double cellX = box.getX() + (xStep * pos.col()) + margin;
        double cellY = box.getY() + (yStep * pos.row()) + margin;
        return new Rectangle2D.Double(cellX,cellY,cellWidth,cellHeight);
    }
}
