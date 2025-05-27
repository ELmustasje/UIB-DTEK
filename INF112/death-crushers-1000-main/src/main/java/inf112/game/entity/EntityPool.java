package inf112.game.entity;

import java.util.ArrayList;
import java.util.List;

/**
 * The EntityPool manages all combat-related entities, including the player and
 * enemies.
 * It handles entity updates.
 */
public class EntityPool {
    private final Player player;
    private final List<Enemy> enemies;

    /**
     * Constructs an EntityPool with a player.
     *
     * @param player The player entity in combat.
     */
    public EntityPool(Player player) {
        this.player = player;
        this.enemies = new ArrayList<>();
    }

    /**
     * Retrieves the player entity.
     * 
     * @return The player in this combat instance.
     */
    public Player player() {
        return player;
    }

    /**
     * Adds an enemy to the pool.
     * 
     * @param enemy The enemy to be added.
     */
    public List<Enemy> enemies() {
        return enemies;
    }

    /**
     * Removes a specific enemy from the pool.
     * 
     * @param enemy The enemy to remove.
     */
    public void removeEnemy(Enemy enemy) {
        enemies.remove(enemy);
    }

    /**
     * Clears all enemies from the pool.
     */
    public void removeAllEnemies() {
        enemies.clear();
    }

    public void addEnemy(Enemy enemy) {
        if (enemy != null && !enemies.contains(enemy))
            enemies.add(enemy);
    }

    /**
     * Determines whether combat is over.
     * Combat ends if all enemies are dead or the player is dead.
     * 
     * @return true if combat is over, false otherwise.
     */
    public boolean isCombatOver() {
        boolean live = false;
        for (Enemy e : enemies)
			if (!e.isDead()) {
				live = true;
				break;
			}
        return enemies.isEmpty() || player.isDead() || !live;
    }

}
