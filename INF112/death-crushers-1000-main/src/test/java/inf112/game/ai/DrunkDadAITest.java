package inf112.game.ai;

import inf112.game.interfaces.CombatInterface;
import inf112.game.entity.Enemy;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

class DrunkDadAITest {

    private DrunkDadAI ai;
    private CombatInterface ci;
    private Enemy enemy;

    @BeforeEach
    void setup() {
        ai = new DrunkDadAI();
        ci = mock(CombatInterface.class);
        enemy = mock(Enemy.class);
        when(ci.currentEnemy()).thenReturn(enemy);
    }

    @Test
    void testAttackIntentAttacks() {
        when(enemy.health()).thenReturn(50);
        when(enemy.maxHealth()).thenReturn(100);

        Intent intent = new Intent(IntentType.ATTACK, 10);
        ai.executeMove(ci, intent);

        verify(ci).damagePlayer(10);
    }

    @Test
    void testSheildIntentShields() {
        when(enemy.health()).thenReturn(30);
        when(enemy.maxHealth()).thenReturn(100);

        Intent intent = new Intent(IntentType.SHIELD, 20);
        ai.executeMove(ci, intent);

        verify(ci).shieldEnemy(eq(enemy), eq(20));
    }

    @Test
    void testLowHealthMove() {
        when(enemy.health()).thenReturn(5);
        when(enemy.maxHealth()).thenReturn(100);

        // trigger oppdatering av health internt
        ai.executeMove(ci, new Intent(IntentType.ATTACK, 1));

        Intent intent = ai.prepareNextMove();

        assertEquals(IntentType.ATTACK, intent.type());
        assertTrue(intent.value() >= 30 && intent.value() < 35);
    }

    @Test
    void testHalfHealthMove() {
        when(enemy.health()).thenReturn(40);
        when(enemy.maxHealth()).thenReturn(100);

        ai.executeMove(ci, new Intent(IntentType.ATTACK, 1));

        Intent intent = ai.prepareNextMove();

        assertTrue(intent.type() == IntentType.SHIELD || intent.type() == IntentType.ATTACK);
    }

    @Test
    void testFullHealthMove() {
        when(enemy.health()).thenReturn(100);
        when(enemy.maxHealth()).thenReturn(100);

        ai.executeMove(ci, new Intent(IntentType.ATTACK, 1));

        Intent intent = ai.prepareNextMove();

        assertEquals(IntentType.ATTACK, intent.type());
        assertTrue(intent.value() >= 4 && intent.value() < 9);
    }
}
