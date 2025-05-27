package inf112.game.deck.cards;

import inf112.core.Config;
import inf112.game.entity.Enemy;
import inf112.game.interfaces.CombatInterface;
import inf112.gfx.Sprite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class StrikeCardTest {
    private StrikeCard card;
    private CombatInterface mockCombat;
    private Enemy mockEnemy;

    @BeforeEach
    void setup() {
        Config.TEST = true;
        card = new StrikeCard();
        mockCombat = mock(CombatInterface.class);
        mockEnemy = mock(Enemy.class);

        when(mockCombat.currentEnemy()).thenReturn(mockEnemy);
    }

    @Test
    void testCardCost() {
        assertEquals(1, card.cost());
    }

    @Test
    void testCardSpriteIsCorrect() {
        assertEquals(Sprite.STRIKE_CARD, card.sprite());
    }

    @Test
    void testDescription() {
        assertEquals("Deals 6 damage.", card.description());
    }
}