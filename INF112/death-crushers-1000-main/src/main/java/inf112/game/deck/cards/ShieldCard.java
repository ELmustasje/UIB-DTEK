package inf112.game.deck.cards;

import inf112.game.deck.ACard;
import inf112.game.interfaces.CombatInterface;
import inf112.gfx.Sprite;

/**
 * Basic card type that extends {@link ACard} and provides shield.
 */
public class ShieldCard extends ACard {

    public static final int COST = 1;

    public ShieldCard() {
        super(COST, 1, Sprite.SHIELD_CARD, "Shield");
    }

    @Override
    public void play(CombatInterface ci) {
        ci.shieldPlayer(5);
    }

    @Override
    public String description() {
        return "Gain 5 shield.";
    }

}
