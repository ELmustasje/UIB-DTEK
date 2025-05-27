package inf112.game.deck.cards;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import inf112.core.Config;
import inf112.game.entity.Enemy;
import inf112.game.interfaces.CombatInterface;
import inf112.gfx.Sprite;

public class ProcrastinationCardTest {
    private ProcrastinationCard card;
    private CombatInterface mockCombat;
    private Enemy mockEnemy;

    @BeforeEach
    void setup() {
        Config.TEST = true;
        card = new ProcrastinationCard();
        mockCombat = mock(CombatInterface.class);
        mockEnemy = mock(Enemy.class);

        when(mockCombat.currentEnemy()).thenReturn(mockEnemy);
    }

    @Test
    void testCardCostIs2() {
        assertEquals(1, card.cost());
    }

    @Test
    void testCardSpriteIsCorrect() {
        assertEquals(Sprite.PROCRASTINATION_CARD, card.sprite());
    }
}
