package inf112.game.ai;

import inf112.game.interfaces.CombatInterface;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.*;

class AttackerAITest {

    private AttackerAI attackerAI;
    private CombatInterface combatInterface;

    @BeforeEach
    void setUp() {
        attackerAI = new AttackerAI();
        combatInterface = mock(CombatInterface.class);
    }

    @Test
    void testPrepareNextMoveAlwaysReturnsAttack() {
        assertEquals(IntentType.ATTACK, attackerAI.intent().type());
    }

    @Test
    void testExecuteMoveDealsDamage() {
        attackerAI.executeMove(combatInterface, new Intent(IntentType.ATTACK, 0));
        verify(combatInterface, times(1)).damagePlayer(anyInt());
    }

    @Test
    void testExecuteMoveCanGiveStunCard() {
        AttackerAI attackerSpy = Mockito.spy(attackerAI);

        attackerSpy.executeMove(combatInterface, new Intent(IntentType.ATTACK, 0));
        // verify(combatInterface, times(1)).giveCardToPlayer(any(StunCard.class));
    }
}
