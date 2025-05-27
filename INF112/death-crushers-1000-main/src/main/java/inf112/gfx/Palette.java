package inf112.gfx;

import com.badlogic.gdx.graphics.Color;

import inf112.game.map.nodes.MapNode;

/**
 * A class containing a palette of colors that can be used in the game.
 * Source: https://lospec.com/palette-list/apollo
 */
@SuppressWarnings("unused")
public class Palette {

    // Blues
    public final static Color BLUE_DARKEST = Color.valueOf("#172038");
    public final static Color BLUE_DARKER = Color.valueOf("#253a5e");
    public final static Color BLUE_DARK = Color.valueOf("#3c5e8b");
    public final static Color BLUE = Color.valueOf("#4f8fba");
    public final static Color BLUE_LIGHT = Color.valueOf("#73bed3");
    public final static Color BLUE_LIGHTEST = Color.valueOf("#a4dddb");

    // Greens
    public final static Color GREEN_DARKEST = Color.valueOf("#19332d");
    public final static Color GREEN_DARKER = Color.valueOf("#25562e");
    public final static Color GREEN_DARK = Color.valueOf("#468232");
    public final static Color GREEN = Color.valueOf("#75a743");
    public final static Color GREEN_LIGHT = Color.valueOf("#a8ca58");
    public final static Color GREEN_LIGHTEST = Color.valueOf("#d0da91");

    // Browns
    public final static Color BROWN_DARKEST = Color.valueOf("#4d2b32");
    public final static Color BROWN_DARKER = Color.valueOf("#7a4841");
    public final static Color BROWN_DARK = Color.valueOf("#ad7757");
    public final static Color BROWN = Color.valueOf("#c09473");
    public final static Color BROWN_LIGHT = Color.valueOf("#d7b594");
    public final static Color BROWN_LIGHTEST = Color.valueOf("#e7d5b3");

    // ORANGEs
    public final static Color ORANGE_DARKEST = Color.valueOf("#341c27");
    public final static Color ORANGE_DARKER = Color.valueOf("#602c2c");
    public final static Color ORANGE_DARK = Color.valueOf("#884b2b");
    public final static Color ORANGE = Color.valueOf("#be772b");
    public final static Color ORANGE_LIGHT = Color.valueOf("#de9e41");
    public final static Color ORANGE_LIGHTEST = Color.valueOf("#e8c170");

    // Oranges
    public final static Color RED_DARKEST = Color.valueOf("#241527");
    public final static Color RED_DARKER = Color.valueOf("#411d31");
    public final static Color RED_DARK = Color.valueOf("#752438");
    public final static Color RED = Color.valueOf("#a53030");
    public final static Color RED_LIGHT = Color.valueOf("#cf573c");
    public final static Color RED_LIGHTEST = Color.valueOf("#da863e");

    // Purples
    public final static Color PURPLE_DARKEST = Color.valueOf("#1e1d39");
    public final static Color PURPLE_DARKER = Color.valueOf("#402751");
    public final static Color PURPLE_DARK = Color.valueOf("#7a367b");
    public final static Color PURPLE = Color.valueOf("#a23e8c");
    public final static Color PURPLE_LIGHT = Color.valueOf("#c65197");
    public final static Color PURPLE_LIGHTEST = Color.valueOf("#df84a5");

    // Blacks & Greys
    public final static Color BLACK_DARKEST = Color.valueOf("#090a14");
    public final static Color BLACK_DARKER = Color.valueOf("#10141f");
    public final static Color BLACK_DARK = Color.valueOf("#151d28");
    public final static Color BLACK = Color.valueOf("#202e37");
    public final static Color BLACK_LIGHT = Color.valueOf("#394a50");

    // Grays & Whites
    public final static Color GRAY_DARK = Color.valueOf("#577277");
    public final static Color GRAY = Color.valueOf("#819796");
    public final static Color GRAY_LIGHT = Color.valueOf("#a8b5b2");
    public final static Color GRAY_LIGHTEST = Color.valueOf("#c7cfcc");
    public final static Color WHITE = Color.valueOf("#ebede9");

    // Colors for backgroun, buttons etc.
    public static final Color BACKGROUND = BLUE_DARKEST;
    public static final Color FOREGROUND = WHITE;
    public static final Color FOREGROUND_MUTED = GRAY_LIGHTEST;

    // Colors for map-graph
    public static final Color EDGE = GRAY_LIGHTEST;
    public static final Color NODE_START = GREEN;
    public static final Color NODE_BATTLE = RED;
    public static final Color NODE_SHOP = BLUE_LIGHT;
    public static final Color NODE_REST = BLUE;
    public static final Color NODE_TREASURE = ORANGE;
    public static final Color NODE_ELITE = PURPLE;
    public static final Color NODE_BOSS = BLACK;
    public static final Color NODE_DEFAULT = GRAY;

    // Colors for UI
    public static final Color BUTTON = BLUE_DARKER;
    public static final Color BUTTON_HOVER = BLUE_DARK;
    public static final Color BUTTON_TEXT = WHITE;
    public static final Color SLIDER = BLUE;
    public static final Color SLIDER_BUTTON = BLUE_DARK;
    public static final Color CHECKBOX = GREEN;
    public static final Color CHECKBOX_HOVER = GREEN_DARK;
    public static final Color TEXTBOX_ACTIVE = GRAY_LIGHT;
    public static final Color TEXTBOX_UNACTIVE = GRAY_DARK;
    public static final Color HEALTH = RED_DARK;
    public static final Color SHIELD = BLUE_LIGHT;

    /**
     * Determines the color for a node based on its {@link MapNode#getNodeType()}.
     *
     * @param node the {@link MapNode}
     * @return a LibGDX {@link Color} associated with the node's type
     */
    public static Color getNodeColor(MapNode node) {
		return switch (node.getNodeType()) {
			case START -> Palette.NODE_START;
			case BATTLE -> Palette.NODE_BATTLE;
			case SHOP -> Palette.NODE_SHOP;
			case REST -> Palette.NODE_REST;
			case TREASURE -> Palette.NODE_TREASURE;
			case BOSS -> Palette.NODE_BOSS;
			default -> Palette.NODE_DEFAULT;
		};
    }
}
