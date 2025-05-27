package inf112.game.ai;

import inf112.game.interfaces.CombatInterface;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

class EduroamAITest {

    EduroamAI ai;
    CombatInterface ci;

    @BeforeEach
    void setup() {
        ai = new EduroamAI() {
            @Override
            protected void executeMove(CombatInterface ci, Intent intent) {
                // Skipper kort-relaterte ting
                if (intent.type() == IntentType.ATTACK)
                    ci.damagePlayer(intent.value());
                else if (intent.type() == IntentType.SHIELD)
                    ci.shieldEnemy(ci.currentEnemy(), intent.value());
            }
        };
        ci = mock(CombatInterface.class);
    }

    @Test
    void testPrepareNextMove_returnsIntent() {
        Intent intent = ai.prepareNextMove();
        assertNotNull(intent);
    }

    @Test
    void testExecuteMove_damagesPlayerOnAttack() {
        Intent intent = new Intent(IntentType.ATTACK, 5);
        ai.executeMove(ci, intent);

        verify(ci).damagePlayer(5);
    }

    @Test
    void testExecuteMove_shieldsEnemyOnShield() {
        Intent intent = new Intent(IntentType.SHIELD, 10);
        ai.executeMove(ci, intent);

        verify(ci).shieldEnemy(ci.currentEnemy(), 10);
    }
}