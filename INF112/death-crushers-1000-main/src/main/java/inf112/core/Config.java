package inf112.core;

/**
 * General config class with predetermined values.
 * Used to avoid magic numbers and strings.
 */
public final class Config {
    /**
     * Private constructor to prevent accidental initialization
     */
    private Config() {
    }

    public static boolean TEST = false;
    public static final int FPS = 60;
    public static boolean DEBUG = false;

    public static long RANDOM_SEED = 12L; // balanced seed
    public static int MAP_ROWS = 9; // One starting row, rest is playing rows
    public static final int MAP_COLUMNS = 7;

    // Width and height of actual window when game launches
    public static final int SCREEN_WIDTH = 1920 / 2;
    public static final int SCREEN_HEIGHT = 1080 / 2;

    // Width and height of canvas the game draws to
    public static final int VIEW_WIDTH = 1920;
    public static final int VIEW_HEIGHT = 1080;

    public static final int CARD_W = 256;
    public static final int CARD_H = 416;

    public static final String WINDOW_TITLE = "Death Crushers";
    public static final String GROUP_NAME = "Death Crushers 1000";
    public static final String GAME_NAME = "Warden of Høytek";
    public static final String LIBRARY = "libGDX";

    // If different types of characters are implemented, their starting mana
    // may be adjusted here
    public static final int STARTING_MANA = 3;

    // EnemyFactory parameters
    public static final int MULTIPLE_ENEMY_MIN = 2;
    public static final int MULTIPLE_ENEMY_MAX = 4;
}
