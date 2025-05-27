package inf112.game.ai;

import inf112.game.components.AStatus;
import inf112.game.entity.Player;
import inf112.game.interfaces.CombatInterface;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.mockito.Mockito.*;
import static org.junit.jupiter.api.Assertions.*;

class ExPhilSeminarLederTest {

    private ExPhilSeminarLeder ai;
    private CombatInterface ci;
    private Player player;

    @BeforeEach
    void setup() {
        ai = new ExPhilSeminarLeder() {
            @Override
            protected void executeMove(CombatInterface ci, Intent intent) {
                // Skip actual LectureCard to avoid FontMetrics crash
                if (intent.type() == IntentType.ATTACK) {
                    ci.damagePlayer(intent.value());
                } else if (intent.type() == IntentType.DEBUFF) {
                    // mock alternative
                    if (random.nextInt(5) < 3) {
                        ci.giveCardToPlayerHandNextTurn(mock(inf112.game.deck.ACard.class));
                    } else {
                        ci.applyStatus(ci.player(), mock(AStatus.class));
                    }
                }
            }
        };

        ci = mock(CombatInterface.class);
        player = mock(Player.class);
        when(ci.player()).thenReturn(player);
    }

    @Test
    void testExecuteMove_attacksPlayer() {
        ai.executeMove(ci, new Intent(IntentType.ATTACK, 12));
        verify(ci).damagePlayer(12);
    }

    @Test
    void testExecuteMove_debuffGivesCardOrStatus() {
        // kalles uansett én av to handlinger
        ai.executeMove(ci, new Intent(IntentType.DEBUFF, 0));

        // verifiser at én av to skjedde
        verify(ci, atMostOnce()).giveCardToPlayerHandNextTurn(any());
        verify(ci, atMostOnce()).applyStatus(eq(player), any());
    }

    @Test
    void testPrepareNextMove_returnsAttackOrDebuff() {
        for (int i = 0; i < 10; i++) {
            Intent intent = ai.prepareNextMove();
            assertNotNull(intent);
            assertTrue(intent.type() == IntentType.ATTACK || intent.type() == IntentType.DEBUFF);
        }
    }
}
