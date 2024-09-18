package no.uib.inf101.gridview;

import no.uib.inf101.colorgrid.CellColor;
import no.uib.inf101.colorgrid.CellColorCollection;
import no.uib.inf101.colorgrid.CellPosition;
import no.uib.inf101.colorgrid.IColorGrid;

import javax.swing.*;
import java.awt.*;
import java.awt.geom.Rectangle2D;

public class GridView extends JPanel {
    private static final double OUTERMARGIN = 30;
    private static final Color MARGINCOLOR = Color.lightGray;
    static CellPositionToPixelConverter CTP;

  // TODO: Implement this class
    IColorGrid grid;
    public GridView(IColorGrid grid){
        this.setPreferredSize(new Dimension(400,300));
        this.grid = grid;
    }
    @Override
    public void paintComponent(Graphics g){
        super.paintComponent(g);
        Graphics2D g2 = (Graphics2D) g;
        drawGrid(g2);
        drawCells(g2,grid,CTP);
    }
    private void drawGrid(Graphics2D g2){
        int height = this.getHeight();
        int width = this.getWidth();
        g2.setColor(MARGINCOLOR);
        Rectangle2D box = new Rectangle2D.Double(OUTERMARGIN,OUTERMARGIN,width-OUTERMARGIN*2,height-OUTERMARGIN*2);
        g2.fill(box);
        CTP = new CellPositionToPixelConverter(box,grid,OUTERMARGIN);
    }

    private static void drawCells(Graphics2D g2, CellColorCollection cells, CellPositionToPixelConverter CTP){
        for(CellColor cell : cells.getCells()){
            if(cell.color() == null){
                g2.setColor(Color.darkGray);
            }
            else {
                g2.setColor(cell.color());
            }
            Rectangle2D rect = CTP.getBoundsForCell(cell.cellPosition());
            g2.fill(rect);
        }
    }
}
