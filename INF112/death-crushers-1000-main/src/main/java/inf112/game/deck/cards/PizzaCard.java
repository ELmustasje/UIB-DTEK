package inf112.game.deck.cards;

import inf112.game.deck.ACard;
import inf112.game.interfaces.CombatInterface;
import inf112.gfx.Sprite;

/**
 * This card gives the player 2 energy points.
 */
public class PizzaCard extends ACard {

    public static final int COST = 0;

    public PizzaCard() {
        super(COST, 7, Sprite.PIZZA_CARD, "Pizza");
    }

    @Override
    public void play(CombatInterface ci) {
        ci.addEnergy(2);
    }

    @Override
    public String description() {
        return "Gain 2 energy.";
    }

}
