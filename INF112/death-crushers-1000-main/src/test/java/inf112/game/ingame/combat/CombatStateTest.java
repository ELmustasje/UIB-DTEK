package inf112.game.ingame.combat;

import inf112.core.state.StateManager;
import inf112.game.components.AStatus;
import inf112.game.deck.ACard;
import inf112.game.entity.AEntity;
import inf112.game.entity.Enemy;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

import static org.mockito.Mockito.*;

class CombatStateTest {

    private CombatState combatState;
    private StateManager stateManager;

    @BeforeEach
    void setUp() {
        this.combatState = mock(CombatState.class);
        this.stateManager = mock(StateManager.class);
    }

    @Test
    void testInit() {
        assertDoesNotThrow(() -> combatState.init(stateManager));
    }

    @Test
    void testUpdate() {
        assertDoesNotThrow(() -> combatState.update());
    }

    @Test
    void testClean() {
        assertDoesNotThrow(() -> combatState.clean());
    }

    @Test
    void testShakeCamera() {
        assertDoesNotThrow(() -> combatState.shakeCamera());
    }

    @Test
    void testDamagePlayer() {
        assertDoesNotThrow(() -> combatState.damagePlayer(3));
    }

    @Test
    void testShieldEnemy() {
        assertDoesNotThrow(() -> combatState.shieldEnemy(mock(Enemy.class), 5));
    }

    @Test
    void testGiveCardToPlayerHandNextTurn() {
        assertDoesNotThrow(() -> combatState.giveCardToPlayerHandNextTurn(mock(ACard.class)));
    }

    @Test
    void testDamageEnemy() {
        assertDoesNotThrow(() -> combatState.damageEnemy(mock(Enemy.class), 5));
    }

    @Test
    void testShieldPlayer() {
        assertDoesNotThrow(() -> combatState.shieldPlayer(3));
    }

    @Test
    void testApplyStatus() {
        assertDoesNotThrow(() -> combatState.applyStatus(mock(AEntity.class), mock(AStatus.class)));
    }

    @Test
    void testGiveRandomCards() {
        assertDoesNotThrow(() -> combatState.giveRandomCards(2));
    }

    @Test
    void testAddEnergy() {
        assertDoesNotThrow(() -> combatState.addEnergy(2));
    }

    @Test
    void testDiscardNCards() {
        assertDoesNotThrow(() -> combatState.discardNCards(2));
    }

    @Test
    void testGiveGold() {
        assertDoesNotThrow(() -> combatState.giveGold(10));
    }

}
