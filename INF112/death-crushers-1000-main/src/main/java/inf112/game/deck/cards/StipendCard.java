package inf112.game.deck.cards;

import inf112.game.deck.ACard;
import inf112.game.interfaces.CombatInterface;
import inf112.gfx.Sprite;

public class StipendCard extends ACard {

    public static final int COST = 2;

    public StipendCard() {
        super(COST, 2, Sprite.STIPEND_CARD, "Stipend");
    }

    @Override
    public void play(CombatInterface ci) {
        ci.giveGold(30);
    }

    @Override
    public String description() {
        return "Gain 30 gold.";
    }

}
