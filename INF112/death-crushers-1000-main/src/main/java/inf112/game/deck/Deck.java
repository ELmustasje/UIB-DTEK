package inf112.game.deck;

import inf112.core.Config;
import inf112.game.Game;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.reflections.Reflections;

/**
 * Class that represents a deck. Holds a hand pile, draw pile and discard pile.
 * To be used in the {@link Game} class.
 */
public class Deck {

    private final List<ACard> handPile;
    private List<ACard> drawPile;
    private final List<ACard> discardPile;

    /**
     * Constructs a new deck with empty piles.
     */
    public Deck() {
        handPile = new ArrayList<>();
        drawPile = new ArrayList<>();
        discardPile = new ArrayList<>();
    }

    /**
     * Constructs a new deck with a predefined drawPile
     *
     * @param drawPile List object containing {@link ACard}s
     */
    public Deck(List<ACard> drawPile) {
        this();
        this.drawPile = drawPile;
    }

    /**
     * Draws a card from the draw pile into the hand pile.
     * Removes the drawn card from the draw pile.
     * May return {@code null} if draw card is empty.
     * 
     * @return ACard most recently drawn card.
     */
    public ACard draw() {
        if (drawPile.isEmpty())
            return null;

        ACard drawnCard = drawPile.get(drawPile.size() - 1);
        if (drawnCard == null) {
            return null;
        }

        handPile.add(drawnCard);
        drawPile.remove(drawnCard);
        return drawnCard;
    }

    /**
     * Adds cards from hand pile and discard pile to the draw pile and reshuffles.
     */
    public void reShuffle() {
        drawPile.addAll(discardPile);
        discardPile.clear();
        drawPile.addAll(handPile);
        handPile.clear();

        Collections.shuffle(drawPile);
    }

    /**
     * Method to call when a card from the deck is played.
     * Removes the card from the hand pile and adds it to the discard pile.
     * To be called between rounds.
     * 
     * @param card {@link ACard} that is used from hand pile
     */
    public void playCard(ACard card) {
        handPile.remove(card);
        discardPile.add(card);
    }

    /**
     * Moves the card from the hand pile to the discard pile.
     * 
     * @param card card to discard
     */
    public void discardCard(ACard card) {
        discardPile.add(card);
        handPile.remove(card);
    }

    /**
     * Adds a given card to the deck. May be used when obtaining new cards.
     * 
     * @param card {@link ACard} to be added to deck
     * @throws IllegalArgumentException if card is null
     */
    public void addToDeck(ACard card) {
        if (card == null) {
            throw new IllegalArgumentException("Card cannot be null");
        }
        drawPile.add(card);
    }

    public List<ACard> getHandPile() {
        return handPile;
    }

    public List<ACard> getDrawPile() {
        return drawPile;
    }

    public List<ACard> getDiscardPile() {
        return discardPile;
    }

    /**
     * Shuffles cards in discard pile into draw pile. Does not affect hand.
     */
    public void reShuffleDiscard() {
        drawPile.addAll(discardPile);
        discardPile.clear();
        Collections.shuffle(drawPile);
    }

    /**
     * Get all cards in the game.
     * 
     * @return List of all cards in the game
     */
    public static List<ACard> getAllCards() {
        List<ACard> cards = new ArrayList<>();
        Reflections reflections = new Reflections("inf112.game.deck.cards");
        Set<Class<? extends ACard>> subTypes = reflections.getSubTypesOf(ACard.class);

        for (Class<? extends ACard> cardClass : subTypes)
            try {
                ACard c = cardClass.getDeclaredConstructor().newInstance();
                if (!c.isDebuff() || Config.DEBUG)
                    cards.add(c);
            } catch (Exception e) {
                System.out.println("error: " + e.getMessage());
            }
        return cards;
    }

    /**
     * Get n random cards from all possible cards
     * 
     * @param n
     * @return
     */
    public static List<ACard> getNRandomCards(int n) {
        List<ACard> cards = Deck.getAllCards();

        Collections.shuffle(cards);
        return cards.subList(0, n);
    }

    public void holdCard(ACard card) {
        handPile.remove(card);
        drawPile.add(card);
    }
}
