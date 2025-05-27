package inf112.game.deck.cards;

import inf112.game.deck.ACard;
import inf112.game.interfaces.CombatInterface;
import inf112.gfx.Sprite;

/**
 * This card costs 3 energy, and gives the player 999 shields.
 */
public class WaffleThursdayCard extends ACard {

    public static final int COST = 3;

    public WaffleThursdayCard() {
        super(COST, 5, Sprite.WAFFLE_THURSDAY_CARD, "Vaffeltorsdag");
    }

    @Override
    public void play(CombatInterface ci) {
        ci.shieldPlayer(999);
    }

    @Override
    public String description() {
        return "Gain 999 shield.";
    }


}
