package inf112.game.deck;

import inf112.core.Config;
import inf112.game.deck.cards.ShieldCard;
import inf112.game.deck.cards.StrikeCard;
import inf112.game.deck.cards.StunCard;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

public class DeckTest {

    private Deck deck;

    @BeforeEach
    public void setUp() {
        Config.TEST = true;
        List<ACard> startingDeck = new ArrayList<>();
        for (int i = 0; i < 5; i++) {
            startingDeck.add(new StrikeCard());
        }
        deck = new Deck(startingDeck);
    }

    @Test
    public void testDrawCard() {
        ACard drawnCard = deck.draw();
        assertNotNull(drawnCard, "Drawn card should not be null.");
        assertEquals(1, deck.getHandPile().size(), "Hand pile should contain 1 card after drawing.");

        for (int i = 0; i < 4; i++) // Draw the rest of the cards
            deck.draw();

        assertNull(deck.draw(), "Drawn card should be null when draw pile is empty.");
        assertEquals(0, deck.getDrawPile().size(), "Draw pile should be empty after drawing all cards.");

        deck.getDrawPile().add(null);
        drawnCard = deck.draw();
        assertNull(drawnCard, "Drawing a null card should return null");

        // Since drawnCard is null, the card is not removed
        assertEquals(1, deck.getDrawPile().size(), "Null card should not be removed from drawPile");
        assertFalse(deck.getHandPile().contains(null), "Hand pile should not contain a null card");
    }

    @Test
    public void testPlayCard() {
        ACard drawnCard = deck.draw();
        assertNotNull(drawnCard, "Drawn card should not be null.");
        int sizeHandBefore = deck.getHandPile().size();
        deck.playCard(drawnCard);
        int sizeHandAfter = deck.getHandPile().size();
        assertEquals(sizeHandBefore - 1, sizeHandAfter, "Hand pile should decrease by one after playing.");
    }

    @Test
    public void testPlayCardMovesToDiscardPile() {
        ACard drawnCard = deck.draw();
        assertNotNull(drawnCard, "Drawn card should not be null.");
        int sizeDiscardBefore = deck.getDiscardPile().size();
        deck.playCard(drawnCard);
        int sizeDiscardAfter = deck.getDiscardPile().size();
        assertEquals(sizeDiscardBefore + 1, sizeDiscardAfter, "Discard pile should increase by one after playing.");
    }

    @Test
    void testAddCardToDeck() {
        ACard newCard = new StrikeCard();
        deck.addToDeck(newCard);
        assertEquals(6, deck.getDrawPile().size(), "Draw pile should have 6 cards after adding a new card.");

        assertThrows(IllegalArgumentException.class, () -> {
            deck.addToDeck(null);
        });
    }

    @Test
    void testReShuffle() {
        while (!deck.getDrawPile().isEmpty()) {
            deck.draw();
        }

        for (ACard card : new ArrayList<>(deck.getHandPile())) {
            deck.playCard(card);
        }

        assertEquals(5, deck.getDiscardPile().size(), "All cards should now be in the discard pile.");
        deck.reShuffle();

        assertEquals(5, deck.getDrawPile().size(), "All cards should be back in the draw pile.");
        assertEquals(0, deck.getHandPile().size(), "Hand pile should still be empty after reshuffling.");
        assertEquals(0, deck.getDiscardPile().size(), "Discard pile should be empty after reshuffling.");
    }

    @Test
    void testReShuffleWithRandomnessCheck() {
        while (!deck.getDrawPile().isEmpty()) {
            deck.draw();
        }

        for (ACard card : new ArrayList<>(deck.getHandPile())) {
            deck.playCard(card);
        }

        List<ACard> preShuffleOrder = new ArrayList<>(deck.getDiscardPile());
        deck.reShuffle();

        assertEquals(5, deck.getDrawPile().size(), "All cards should be back in the draw pile.");
        assertEquals(0, deck.getHandPile().size(), "Hand pile should still be empty after reshuffling.");
        assertEquals(0, deck.getDiscardPile().size(), "Discard pile should be empty after reshuffling.");

        boolean shuffledProperly = false;
        for (int i = 0; i < 5; i++) {
            deck.reShuffle();
            if (!preShuffleOrder.equals(deck.getDrawPile())) {
                shuffledProperly = true;
                break;
            }
        }
        assertTrue(shuffledProperly, "Deck should have been shuffled, order should change.");
    }

    @Test
    public void testDiscardCard() {
        ACard card = new StrikeCard();
        deck.getHandPile().add(card);
        deck.discardCard(card);
        assertFalse(deck.getHandPile().contains(card), "Card should be removed from hand after discarding");
        assertTrue(deck.getDiscardPile().contains(card), "Card should be added to discard after discarding");
    }

    @Test
    public void testReShuffleDiscard() {
        ACard card1 = new StrikeCard();
        ACard card2 = new StrikeCard();
        deck.getDiscardPile().add(card1);
        deck.getDiscardPile().add(card2);
        int drawPileSizeBefore = deck.getDrawPile().size();
        deck.reShuffleDiscard();
        assertTrue(deck.getDiscardPile().isEmpty(), "Discard pile should be empty after reShuffleDiscard");
        assertEquals(drawPileSizeBefore + 2, deck.getDrawPile().size(),
                "Draw pile should have increased by 2 after reShuffleDiscard");
        assertTrue(deck.getDrawPile().contains(card1), "Draw pile should contain card1 after reShuffleDiscard");
        assertTrue(deck.getDrawPile().contains(card2), "Draw pile should contain card2 after reShuffleDiscard");
    }

    @Test
    void testGetAllCards() {
        List<ACard> cards = Deck.getAllCards();

        assertNotNull(cards, "getAllCards() should never return null.");
        assertFalse(cards.isEmpty(), "getAllCards() should not be empty.");

        // Example: check if specific card types exist
        boolean containsStrikeCard = cards.stream().anyMatch(card -> card instanceof StrikeCard);
        boolean containsShieldCard = cards.stream().anyMatch(card -> card instanceof ShieldCard);
        boolean containsStunCard = cards.stream().anyMatch(card -> card instanceof StunCard);
        // boolean containsThrottledCard = cards.stream().anyMatch(card -> card
        // instanceof ThrottledCard);

        assertTrue(containsStrikeCard, "getAllCards() should contain StrikeCard.");
        assertTrue(containsShieldCard, "getAllCards() should contain ShieldCard.");
        assertTrue(containsStunCard, "getAllCards() should contain StunCard.");
        // assertTrue(containsThrottledCard, "getAllCards() should contain
        // ThrottledCard.");
    }

    @Test
    void testGetRandomCardsReturnsCorrectSize() {
        List<ACard> randomCards = Deck.getNRandomCards(3);
        assertNotNull(randomCards);
        assertEquals(3, randomCards.size(), "Should return exactly 5 random cards");
    }

    @Test
    void testGetRandomCardsReturnsUniqueInstances() {
        List<ACard> randomCards1 = Deck.getNRandomCards(5);
        List<ACard> randomCards2 = Deck.getNRandomCards(5);

        assertNotEquals(randomCards1, randomCards2, "Each call should return different random cards");
    }
}
