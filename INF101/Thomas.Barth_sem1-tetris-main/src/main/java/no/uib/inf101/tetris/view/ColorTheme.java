package no.uib.inf101.tetris.view;

import java.awt.*;

public interface ColorTheme {

    /**
     * Returns the color for a specific cell character.
     *
     * @param ch the character value representing a cell
     * @return the color for the cell
     */
    Color getCellColor(char ch);

    /**
     * Returns the color for the frame.
     *
     * @return the color for the frame
     */
    Color getFrameColor();

    /**
     * Returns the background color.
     *
     * @return the background color
     */
    Color getBackgroundColor();

    /**
     * Returns the color for the game over rectangle.
     *
     * @return the color for the game over rectangle
     */
    Color getGameOverRectColor();

    /**
     * Returns the color for the game over text.
     *
     * @return the color for the game over text
     */
    Color getGameOverTextColor();
}