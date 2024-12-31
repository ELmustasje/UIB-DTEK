package labyrinth.view;

import java.awt.Color;

public class DefaultColorTheme implements ColorTheme {

    @Override
    public Color getCellColor(char c) {
        Color color = switch(c) {
            case ' ' -> Color.WHITE;
            case '*' -> Color.BLACK;
            case 's' -> Color.BLUE;
            case 'm' -> Color.RED;
            case 'p' -> Color.GREEN;
            default -> throw new IllegalArgumentException(
                    "No available color for '" + c + "'");
        };
        return color;
    }

    @Override
    public Color getFrameColor() {
        return new Color(0, 0, 0, 0);
    }

    @Override
    public Color getBackgroundColor() {
        return null;
    }

}
