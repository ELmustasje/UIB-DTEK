package no.uib.inf101.tetris.view;

import java.awt.*;

public class DefaultColorTheme implements ColorTheme {
    /**
     * Returns the color for a specific cell in the game grid based on the given character.
     *
     * @param ch the character representing the cell in the game grid
     * @return the color for the specified cell
     * @throws IllegalArgumentException if the character does not have an associated color
     */
    @Override
    public Color getCellColor(char ch) {
        return switch (ch) {
            case 'L' -> Color.green;
            case 'J' -> Color.red;
            case 'S' -> Color.blue;
            case 'Z' -> Color.yellow;
            case 'T' -> Color.magenta;
            case 'I' -> Color.CYAN;
            case 'O' -> Color.ORANGE;
            case 'r' -> Color.white;
            case '-' -> Color.black;
            default -> throw new IllegalArgumentException("No availiable Color for '" + ch + "'");
        };
    }

    /**
     * Returns the color for the frame around the game grid.
     *
     * @return the color for the frame
     */
    @Override
    public Color getFrameColor() {
        return new Color(0, 0, 0, 0);
    }

    /**
     * Returns the background color for the game.
     *
     * @return the background color
     */
    @Override
    public Color getBackgroundColor() {
        return null;
    }

    /**
     * Returns the color for the rectangle displayed when the game is over.
     *
     * @return the color for the game over rectangle
     */
    @Override
    public Color getGameOverRectColor() {
        return new Color(0, 0, 0, 128);
    }

    /**
     * Returns the color for the text displayed when the game is over.
     *
     * @return the color for the game over text
     */
    @Override
    public Color getGameOverTextColor() {
        return Color.white;
    }
}