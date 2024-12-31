package labyrinth.view;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;

import javax.swing.JPanel;

import grid.GridCell;
import grid.GridDimension;
import labyrinth.model.GameState;

public class LabyrinthView extends JPanel {

    private static final double OUTERMARGIN = 20;
    private static final double INNERMARGIN = 2;
    private static final double PREFERREDSIDESIZE = 30;
    private ViewableTetrisModel model;
    private ColorTheme colors;

    // Constructor
    public LabyrinthView(ViewableTetrisModel model) {
        this.model = model;
        this.colors = new DefaultColorTheme();
        this.setBackground(this.colors.getBackgroundColor());
        this.setFocusable(true);
        this.setPreferredSize(getDefaultSize(this.model.getDimension()));
    }

    private static Dimension getDefaultSize(GridDimension gd) {
        int width = (int) (PREFERREDSIDESIZE * gd.cols() + INNERMARGIN * (gd.cols() + 1) + 2 * OUTERMARGIN);
        int height = (int) (PREFERREDSIDESIZE * gd.rows() + INNERMARGIN * (gd.cols() + 1) + 2 * OUTERMARGIN);
        return new Dimension(width, height);
    }

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);
        Graphics2D g2 = (Graphics2D) g;

        drawGame(g2);
        if (this.model.getGameState() == GameState.GAME_OVER) {
            drawGameOver(g2);
        }
    }

    private void drawGameOver(Graphics2D g2) {
        g2.setColor(new Color(0, 0, 0, 128));
        g2.fill(this.getBounds());
        g2.setColor(Color.WHITE);
        g2.setFont(new Font("Monospaced", Font.BOLD, 50));
        Inf101Graphics.drawCenteredString(g2, "GAME OVER", getBounds());
    }

    private void drawGame(Graphics2D g2) {
        Rectangle2D box = new Rectangle2D.Double(
                OUTERMARGIN,
                OUTERMARGIN,
                this.getWidth() - OUTERMARGIN * 2,
                this.getHeight() - OUTERMARGIN * 2);

        g2.setColor(this.colors.getFrameColor());
        g2.fill(box);

        CellPositionToPixelConverter converter = new CellPositionToPixelConverter(
                box, this.model.getDimension(), INNERMARGIN);
        drawCells(g2, this.model.getTilesOnBoard(), converter, this.colors);
        drawCells(g2, this.model.getPlayer(), converter, this.colors);
        drawCells(g2, this.model.getMonster(), converter, this.colors);

        drawScore(g2);
    }

    private void drawScore(Graphics2D g2) {
        g2.setFont(new Font("Monospaced", Font.BOLD, 20));
        g2.setColor(Color.BLACK);
        String scoreText = "Score: " + this.model.getScore();
        g2.drawString(scoreText, 20, 20);
    }

    private static void drawCells(Graphics2D g2, Iterable<GridCell<Character>> cells,
                                  CellPositionToPixelConverter converter, ColorTheme colors) {
        for (GridCell<Character> cell : cells) {
            Rectangle2D box = converter.getBoundsForCell(cell.pos());
            Color color = colors.getCellColor(cell.value());
            g2.setColor(color);
            g2.fill(box);
        }
    }
}
