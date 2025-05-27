package inf112.game.deck.cards;

import inf112.game.deck.ACard;
import inf112.game.interfaces.CombatInterface;
import inf112.gfx.Sprite;

/**
 * Basic attack card that extends {@link ACard} and inflicts damage
 */
public class StrikeCard extends ACard {
    public static final int COST = 1;

    public StrikeCard() {
        super(COST, 1, Sprite.STRIKE_CARD, "Strike");
    }

    @Override
    public void play(CombatInterface ci) {
        ci.damageEnemy(ci.currentEnemy(), 6);
    }

    @Override
    public String description() {
        return "Deals 6 damage.";
    }

}
