package inf112.game.deck;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

public class CardTypeTest {
    @Test
    void testCardTypeValues() {
        CardType[] values = CardType.values();
        assertEquals(3, values.length);
        assertArrayEquals(new CardType[] { CardType.ATTACK, CardType.DEFEND, CardType.STATUS }, values);
    }

    @Test
    void testCardTypeValueOf() {
        assertEquals(CardType.ATTACK, CardType.valueOf("ATTACK"));
        assertEquals(CardType.DEFEND, CardType.valueOf("DEFEND"));
        assertEquals(CardType.STATUS, CardType.valueOf("STATUS"));
    }
}
