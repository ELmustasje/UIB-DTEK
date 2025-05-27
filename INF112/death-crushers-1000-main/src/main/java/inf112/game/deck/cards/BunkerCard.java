package inf112.game.deck.cards;

import inf112.game.deck.ACard;
import inf112.game.interfaces.CombatInterface;
import inf112.gfx.Sprite;

public class BunkerCard extends ACard {

    public static final int COST = 2;

    public BunkerCard() {
        super(COST, 3, Sprite.BUNKER_CARD, "Bunker");
    }

    @Override
    public void play(CombatInterface ci) {
        ci.shieldPlayer(20);
        ci.giveRandomCards(2);
    }

    @Override
    public String description() {
        return "Gain 20 shield. Draw 2 cards.";
    }

}
