
package inf112.game.deck.cards;

import inf112.game.deck.ACard;
import inf112.game.interfaces.CombatInterface;
import inf112.gfx.Sprite;

public class CoffeeBreakCard extends ACard {

    public static final int COST = 0;

    public CoffeeBreakCard() {
        super(COST, 3, Sprite.COFFEE_BREAK_CARD, "Coffee Break");
    }

    @Override
    public void play(CombatInterface ci) {
        ci.discardNCards(2);
        ci.giveRandomCards(2);
    }

    @Override
    public String description() {
        return "Discard 2 cards. Draw 2 cards.";
    }

}
