package inf112.game.deck.cards;

import inf112.core.Config;
import inf112.gfx.Sprite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class EnergyDrinkCardTest {
    private EnergyDrinkCard card;

    @BeforeEach
    void setup() {
        Config.TEST = true;
        card = new EnergyDrinkCard();

    }

    @Test
    void testCardCost() {
        assertEquals(1, card.cost());
    }

    @Test
    void testCardSpriteIsCorrect() {
        assertEquals(Sprite.ENERGY_DRINK_CARD, card.sprite());
    }

    @Test
    void description() {
        assertEquals("Become Energized. +25% damage 2 turn.", card.description());
    }
}