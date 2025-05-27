package inf112.game.entity;

import inf112.core.Config;
import inf112.game.ai.AttackerAI;
import inf112.game.ai.EnemyAI;
import inf112.game.ai.IntentType;
import inf112.game.interfaces.CombatInterface;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

class EnemyAITest {

    private EnemyAI attackerAI;
    private CombatInterface combatInterface;

    @BeforeEach
    void setUp() {
        Config.TEST = true;
        attackerAI = new AttackerAI();
        combatInterface = Mockito.mock(CombatInterface.class);
    }

    @Test
    void testAttackerAITurn_CallsDamagePlayer() {
        attackerAI.turn(combatInterface);
        verify(combatInterface, times(1)).damagePlayer(Mockito.anyInt());
    }

    @Test
    void testPrepareNextMove_UpdatesMoveCorrectly() {
        IntentType nextMove = attackerAI.intent().type();
        assertEquals(IntentType.ATTACK, nextMove);
    }
}
