package inf112.game.entity;

import inf112.game.map.nodes.NodeType;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for the {@link EnemyFactory} class.
 */
public class EnemyFactoryTest {

    /**
     * Tests that generating enemies for a BOSS node returns null because the switch
     * falls through to default.
     */
    @Test
    public void testGenerateEnemiesBossReturnsNull() {
        List<Enemy> enemies = EnemyFactory.generateEnemies(1, NodeType.BOSS, 1);
        assertNotNull(enemies, "generetaEnemies should return null for NodeType.BOSS due to fall-through.");
    }

    /**
     * Tests that calling generetaEnemies() with a null NodeType throws a
     * NullPointerException.
     */
    @Test
    public void testGenerateEnemiesNullType() {
        assertThrows(NullPointerException.class, () -> {
            EnemyFactory.generateEnemies(1, null, 1);
        }, "generetaEnemies() should throw NullPointerException when NodeType is null.");
    }
}
