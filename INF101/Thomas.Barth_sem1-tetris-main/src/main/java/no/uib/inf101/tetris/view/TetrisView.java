package no.uib.inf101.tetris.view;

import no.uib.inf101.grid.GridCell;
import no.uib.inf101.tetris.model.GameState;

import javax.imageio.ImageIO;
import javax.swing.*;
import java.awt.*;
import java.awt.geom.Rectangle2D;
import java.io.File;
import java.io.IOException;

/**
 * The TetrisView class is responsible for rendering the Tetris game on the screen.
 * It extends the JPanel class and overrides the paintComponent method to draw the game.
 * It also has methods to draw the game over screen and the game board.
 */
public class TetrisView extends JPanel {

    private final ViewableTetrisModel tetrisModel;
    private final ColorTheme colorTheme;
    private static final int MARGIN = 2;
    private static final int PREFFEREDSIZE = 35;

    /**
     * Constructs a TetrisView object with the given ViewableTetrisModel.
     * Sets the preferred size of the panel based on the size of the tetrisModel.
     *
     * @param tetrisModel The ViewableTetrisModel to be rendered.
     */
    public TetrisView(ViewableTetrisModel tetrisModel){
        this.tetrisModel = tetrisModel;
        this.colorTheme = new DefaultColorTheme();
        this.setPreferredSize(
                new Dimension(
                        (MARGIN+PREFFEREDSIZE) * tetrisModel.cols() + MARGIN,
                        (MARGIN+PREFFEREDSIZE)*tetrisModel.rows() + MARGIN));
    }

    /**
     * Overrides the paintComponent method to draw the game.
     * Calls the drawGame method to draw the game board and the falling tetromino.
     * If the game state is GAME_OVER, calls the drawGameOver method to draw the game over screen.
     *
     * @param g The Graphics object to be used for drawing.
     */
    @Override
    protected void paintComponent(Graphics g) {
        drawGame((Graphics2D) g);
        if(tetrisModel.getGameState() == GameState.GAME_OVER){
            drawGameOver((Graphics2D) g);
        }
    }

    /**
     * Draws the game over screen.
     * Fills a rectangle with the game over color and draws the "GAME OVER" text in the center of the panel.
     *
     * @param g2 The Graphics2D object to be used for drawing.
     */
    private void drawGameOver(Graphics2D g2){
        g2.setColor(colorTheme.getGameOverRectColor());
        Rectangle2D rect = new Rectangle(0,0,this.getWidth(),this.getHeight());
        g2.fill(rect);
        Font myFont = new Font("GameOver",Font.BOLD,40);
        g2.setFont(myFont);
        FontMetrics met = g2.getFontMetrics();
        int x = (this.getWidth() - met.stringWidth("GAME OVER")) / 2;
        int y =(this.getHeight() - met.getHeight()) / 2;
        g2.setColor(colorTheme.getGameOverTextColor());
        g2.drawString("GAME OVER", x, y);
    }

    /**
     * Draws the game board and the falling tetromino.
     * Fills the background with the background color.
     * Uses the CellPositionToPixelConverter to convert cell positions to pixel coordinates.
     * Calls the drawCells method to draw the cells on the board.
     *
     * @param g2 The Graphics2D object to be used for drawing.
     */
    private void drawGame(Graphics2D g2){
        CellPositionToPixelConverter CTP;
        double width = this.getWidth() - 2 * MARGIN;
        double height = this.getHeight() - 2 * MARGIN;
        Rectangle2D board = new Rectangle2D.Double(MARGIN, MARGIN, width, height);
        g2.setColor(getBackground());
        g2.fill(board);
        CTP = new CellPositionToPixelConverter(board, tetrisModel.getDimention(),(double)MARGIN);

        drawCells(g2, tetrisModel.getTilesOnBoard(),CTP,colorTheme);
        //activate chadMode!! drawChadCells(g2, tetrisModel.getTilesOnBoard(), CTP,colorTheme);
        drawCells(g2, tetrisModel.fallingTetromino(), CTP,colorTheme);

    }

    /**
     * Draws the cells on the board.
     * Sets the color of each cell based on the cell value and fills the corresponding rectangle.
     *
     * @param g2 The Graphics2D object to be used for drawing.
     * @param cells The iterable collection of cells to be drawn.
     * @param CPT The CellPositionToPixelConverter object to convert cell positions to pixel coordinates.
     * @param colorTheme The ColorTheme object to get the color for each cell value.
     */
    private static void drawCells(Graphics2D g2, Iterable<GridCell<Character>> cells, CellPositionToPixelConverter CPT, ColorTheme colorTheme){

        for (GridCell<Character> cell : cells) {
            g2.setColor(colorTheme.getCellColor(cell.value()));
            g2.fill(CPT.getBoundsForCell(cell.pos()));
        }
    }

    //chadMode
    private static void drawChadCells(Graphics2D g2, Iterable<GridCell<Character>> cells, CellPositionToPixelConverter CPT, ColorTheme colorTheme){
        try {
            Image img = ImageIO.read(new File("src/main/java/no/uib/inf101/tetris/midi/Giga_Chad.jpg"));
            for (GridCell<Character> cell : cells) {
                Rectangle2D rect = CPT.getBoundsForCell(cell.pos());
                if(cell.value() == '-') {
                    g2.drawImage(img, (int) rect.getX(), (int) rect.getY(), (int) rect.getWidth(), (int) rect.getHeight(), null);
                }else {
                    g2.setColor(colorTheme.getCellColor(cell.value()));
                    g2.fill(CPT.getBoundsForCell(cell.pos()));
                }

            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}