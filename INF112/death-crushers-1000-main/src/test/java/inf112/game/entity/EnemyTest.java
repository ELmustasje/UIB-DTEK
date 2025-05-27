package inf112.game.entity;

import inf112.game.ai.AttackerAI;
import inf112.game.ai.EnemyAI;
import inf112.game.ai.IntentType;
import inf112.game.interfaces.CombatInterface;
import inf112.gfx.Sprite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class EnemyTest {

    private Enemy attackerEnemy;

    @BeforeEach
    void setUp() {
        attackerEnemy = new Enemy(50, 10, "Goblin", new AttackerAI(), Sprite.TEST);
    }

    @Test
    void testEnemyInitialization() {
        assertEquals(50, attackerEnemy.health());
        assertEquals(10, attackerEnemy.shield());
        assertEquals("Goblin", attackerEnemy.getName());
        assertTrue(attackerEnemy.ai() instanceof AttackerAI);
    }

    @Test
    void testTakeTurnCallsTurnMethod() {
        // Create a mock EnemyAI and CombatInterface.
        EnemyAI mockAI = mock(EnemyAI.class);
        CombatInterface mockCI = mock(CombatInterface.class);

        Enemy enemy = new Enemy(50, 10, "TestEnemy", mockAI, Sprite.TEST);
        enemy.takeTurn(mockCI);

        verify(mockAI, times(1)).turn(mockCI);
    }

    @Test
    void testEnemyGetsCorrectNextMove() {
        assertEquals(IntentType.ATTACK, attackerEnemy.intent().type());
    }

    @Test
    void testEnemyDiesWhenTakingLethalDamage() {
        attackerEnemy.dealDamage(100);
        assertEquals(0, attackerEnemy.health());
        assertTrue(attackerEnemy.isDead());
    }

    @Test
    void testEnemySurvivesIfNotEnoughDamage() {
        attackerEnemy.dealDamage(40);
        assertEquals(20, attackerEnemy.health());
        assertFalse(attackerEnemy.isDead());
    }

    @Test
    void testShieldAbsorbsDamage() {
        attackerEnemy.dealDamage(5);
        assertEquals(50, attackerEnemy.health());
        assertEquals(5, attackerEnemy.shield());
    }
}
