package inf112.game.deck.cards;

import inf112.game.deck.ACard;
import inf112.game.interfaces.CombatInterface;
import inf112.gfx.Sprite;

public class ReadCard extends ACard {

    public static final int COST = 0;

    public ReadCard() {
        super(COST, 5, Sprite.READ_CARD, "Read");
    }

    @Override
    public void play(CombatInterface ci) {
        ci.giveRandomCards(3);
    }

    @Override
    public String description() {
        return "Draw 3 cards.";
    }
}
