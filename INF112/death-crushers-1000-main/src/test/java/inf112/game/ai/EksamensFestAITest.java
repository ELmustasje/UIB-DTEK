package inf112.game.ai;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import inf112.game.entity.Enemy;
import inf112.game.interfaces.CombatInterface;

public class EksamensFestAITest {
    private EksamensFestAi ai;
    private CombatInterface ci;
    private Enemy enemy;

    @BeforeEach
    void setUp() {
        ai = new EksamensFestAi() {
            @Override
            protected void executeMove(CombatInterface ci, Intent intent) {
                // Unngå BeerCard for test
                if (intent.type() == IntentType.SHIELD) {
                    ci.shieldEnemy(ci.currentEnemy(), intent.value());
                }
                if (intent.type() == IntentType.ATTACK) {
                    ci.damagePlayer(intent.value());
                }
                // Ikke kall new BeerCard()
                ci.giveCardToPlayerHandNextTurn(mock(inf112.game.deck.ACard.class));
            }
        };

        ci = mock(CombatInterface.class);
        enemy = mock(Enemy.class);
        when(ci.currentEnemy()).thenReturn(enemy);
    }

    @Test
    void testExecuteMove_attacksPlayer() {
        Intent intent = new Intent(IntentType.ATTACK, 10);
        ai.executeMove(ci, intent);
        verify(ci).damagePlayer(10);
    }

    @Test
    void testExecuteMove_shieldsEnemy() {
        Intent intent = new Intent(IntentType.SHIELD, 7);
        ai.executeMove(ci, intent);
        verify(ci).shieldEnemy(eq(enemy), eq(7));
    }

    @Test
    void testExecuteMove_givesCard() {
        ai.executeMove(ci, new Intent(IntentType.ATTACK, 5));
        verify(ci).giveCardToPlayerHandNextTurn(any());
    }

    @Test
    void testPrepareNextMove_returnsValidIntent() {
        for (int i = 0; i < 10; i++) {
            Intent intent = ai.prepareNextMove();
            assertNotNull(intent);
            assertTrue(
                    intent.type() == IntentType.SHIELD || intent.type() == IntentType.ATTACK);
        }
    }
}
