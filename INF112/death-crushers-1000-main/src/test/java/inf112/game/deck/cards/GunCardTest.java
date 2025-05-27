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

class GunCardTest {
    private GunCard card;
    private CombatInterface mockCombat;
    private Enemy mockEnemy;

    @BeforeEach
    void setup() {
        Config.TEST = true;
        card = new GunCard();
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
        assertEquals(Sprite.GUN_CARD, card.sprite());
    }

    @Test
    void testShootingGun() {
        assertEquals("Deal 24 damage. 3 bullets left.", card.description());
        assertFalse(card.unplayable());
        card.play(mockCombat);
        assertEquals("Deal 24 damage. 2 bullets left.", card.description());
        assertFalse(card.unplayable());
        card.play(mockCombat);
        assertEquals("Deal 24 damage. 1 bullets left.", card.description());
        assertFalse(card.unplayable());
        card.play(mockCombat);
        assertTrue(card.unplayable());
        assertEquals("Deal 24 damage. 0 bullets left.", card.description());
    }
}