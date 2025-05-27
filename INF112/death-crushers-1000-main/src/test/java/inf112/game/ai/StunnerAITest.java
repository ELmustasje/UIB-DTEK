package inf112.game.ai;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import inf112.core.Config;
import inf112.game.components.StunStatus;
import inf112.game.interfaces.CombatInterface;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class StunnerAITest {

    private StunnerAI stunnerAI;
    private CombatInterface mockCombatInterface;
    private Intent mockIntent;

    @BeforeEach
    void setUp() {
        Config.TEST = true;
        stunnerAI = spy(new StunnerAI()); // Bruker spy for å mocke metoder
        mockCombatInterface = mock(CombatInterface.class);
        mockIntent = mock(Intent.class);

        when(mockIntent.type()).thenReturn(IntentType.ATTACK);
        when(mockIntent.value()).thenReturn(8);
    }

    @Test
    void testExecuteMoveDamagesPlayer() {
        stunnerAI.executeMove(mockCombatInterface, mockIntent);
        verify(mockCombatInterface).damagePlayer(8); // Sjekker at skade påføres
    }

    @Test
    void testExecuteMoveGivesStunCardOnShortCircuit() {
        doReturn(true).when(stunnerAI).calculateShortCircuit(); // Simulerer at kort skal gis
        stunnerAI.executeMove(mockCombatInterface, mockIntent);

        verify(mockCombatInterface).applyStatus(any(), any(StunStatus.class)); // Sjekker at stun gis
    }

    @Test
    void testPrepareNextMoveReturnsAttackIntent() {
        Intent intent = stunnerAI.prepareNextMove();
        assertEquals(IntentType.ATTACK, intent.type());
        assertTrue(intent.value() >= 6 && intent.value() <= 15); // Sjekker skaderange
    }
}
