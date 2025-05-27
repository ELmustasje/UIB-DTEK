package inf112.game.entity;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.when;

class EntityPoolTest {

    private EntityPool entityPool;
    private Player player;
    private Enemy enemy1;
    private Enemy enemy2;

    @BeforeEach
    void setUp() {
        player = Mockito.mock(Player.class);
        enemy1 = Mockito.mock(Enemy.class);
        enemy2 = Mockito.mock(Enemy.class);
        entityPool = new EntityPool(player);
    }

    @Test
    void testEntityPoolInitialization() {
        assertEquals(player, entityPool.player());
        assertTrue(entityPool.enemies().isEmpty());
    }

    @Test
    void testAddEnemy() {
        entityPool.addEnemy(enemy1);
        List<Enemy> enemies = entityPool.enemies();
        assertEquals(1, enemies.size());
        assertTrue(enemies.contains(enemy1));
    }

    @Test
    void testAddMultipleEnemies() {
        entityPool.addEnemy(enemy1);
        entityPool.addEnemy(enemy2);
        assertEquals(2, entityPool.enemies().size());
    }

    @Test
    void testRemoveEnemy() {
        entityPool.addEnemy(enemy1);
        entityPool.removeEnemy(enemy1);
        assertTrue(entityPool.enemies().isEmpty());
    }

    @Test
    void testRemoveEnemy_NotInPool() {
        entityPool.addEnemy(enemy1);
        entityPool.removeEnemy(enemy2);
        assertEquals(1, entityPool.enemies().size());
    }

    @Test
    void testRemoveAllEnemies() {
        entityPool.addEnemy(enemy1);
        entityPool.addEnemy(enemy2);
        entityPool.removeAllEnemies();
        assertTrue(entityPool.enemies().isEmpty());
    }

    @Test
    void testIsCombatOver_WithNoEnemies() {
        assertTrue(entityPool.isCombatOver());
    }

    @Test
    void testIsCombatOver_WithEnemiesPresent() {
        entityPool.addEnemy(enemy1);
        assertFalse(entityPool.isCombatOver());
    }

    @Test
    void testIsCombatOver_WhenPlayerDies() {
        when(player.isDead()).thenReturn(true);
        assertTrue(entityPool.isCombatOver());
    }

    @Test
    void testIsCombatOver_WhenLastEnemyDies() {
        entityPool.addEnemy(enemy1);
        when(enemy1.isDead()).thenReturn(true);
        entityPool.removeEnemy(enemy1);
        assertTrue(entityPool.isCombatOver());
    }

    @Test
    void testIsCombatOver_WithEnemiesPresentAndPlayerDead() {
        entityPool.addEnemy(enemy1); // Add an enemy so that the list is non-empty.
        when(player.isDead()).thenReturn(true); // Player is dead.
        assertTrue(entityPool.isCombatOver(), "Combat should be over when player is dead, regardless of enemies.");
    }

    @Test
    void testAddingDuplicateEnemy() {
        entityPool.addEnemy(enemy1);
        entityPool.addEnemy(enemy1);

        assertEquals(1, entityPool.enemies().size());
    }

    @Test
    void testAddNullEnemyDoesNothing() {
        entityPool.addEnemy(null);
        assertTrue(entityPool.enemies().isEmpty());
    }

    @Test
    void testRemoveEnemyFromEmptyPoolDoesNotCrash() {
        assertDoesNotThrow(() -> entityPool.removeEnemy(enemy1));
    }

    @Test
    void testRemoveAllEnemiesFromEmptyPoolDoesNotCrash() {
        assertDoesNotThrow(entityPool::removeAllEnemies);
    }
}
