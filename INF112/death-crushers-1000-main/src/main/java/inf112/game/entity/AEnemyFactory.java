package inf112.game.entity;

import java.util.List;
import java.util.Random;

public abstract class AEnemyFactory {

    protected interface Spawner {
        Enemy spawn();
    }

    protected static Enemy lastEnemy = null;

    protected static final Random random = new Random();
    protected static List<Spawner> spawners;

    /**
     * Call at game init to create all enemy classes.
     */
    public static void init() {
    }
}
