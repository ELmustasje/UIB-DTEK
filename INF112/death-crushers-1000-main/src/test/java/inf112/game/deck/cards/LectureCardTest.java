package inf112.game.deck.cards;

import inf112.core.Config;
import inf112.game.deck.ACard;
import inf112.game.entity.Enemy;
import inf112.game.interfaces.CombatInterface;
import inf112.gfx.Sprite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class LectureCardTest {
    private LectureCard card;
    private CombatInterface mockCombat;
    private Enemy mockEnemy;

    @BeforeEach
    void setup() {
        Config.TEST = true;
        card = new LectureCard();
        mockCombat = mock(CombatInterface.class);
        mockEnemy = mock(Enemy.class);

        when(mockCombat.currentEnemy()).thenReturn(mockEnemy);
    }

    @Test
    void testCardSpriteIsCorrect() {
        assertEquals(Sprite.LECTURE_CARD, card.sprite());
    }

    @Test
    void testCardCost() {
        assertEquals(0, card.cost());
    }

    @Test
    void testDescription() {
        assertEquals("Just a boring lecture. Unplayable.", card.description());
    }

}