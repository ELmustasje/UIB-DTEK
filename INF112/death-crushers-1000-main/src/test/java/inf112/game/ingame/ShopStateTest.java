package inf112.game.ingame;

import static org.mockito.Mockito.*;

import inf112.game.deck.ACard;
import inf112.game.deck.Deck;
import inf112.game.entity.Player;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.util.ArrayList;
import java.util.List;

class ShopStateTest {

    private GameManager gameManager;
    private Deck deck;
    private Player player;
    private List<ACard> mockCards;

    @Test
    void setUp() {
        gameManager = mock(GameManager.class);
        deck = mock(Deck.class);
        player = mock(Player.class);

        when(gameManager.deck()).thenReturn(deck);
        when(gameManager.player()).thenReturn(player);
        when(player.getGold()).thenReturn(100); // Assume player has 100 gold

        // Create mock cards for testing
        mockCards = new ArrayList<>();
        for (int i = 0; i < 10; i++) {
            ACard mockCard = mock(ACard.class);
            when(mockCard.price()).thenReturn(10 + i);
            when(mockCard.rarity()).thenReturn(1);
            mockCards.add(mockCard);
        }

        // Mock Deck.getAllCards() using try-with-resources to avoid conflicts
        try (MockedStatic<Deck> mockedDeck = Mockito.mockStatic(Deck.class)) {
            mockedDeck.when(Deck::getAllCards).thenReturn(mockCards);
        }
    }

}
