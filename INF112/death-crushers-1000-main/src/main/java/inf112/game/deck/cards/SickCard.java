package inf112.game.deck.cards;

import inf112.game.deck.ACard;
import inf112.game.interfaces.CombatInterface;
import inf112.gfx.Sprite;

public class SickCard extends ACard {

    public static final int COST = 2;

    public SickCard() {
        super(COST, 3, Sprite.SICK_CARD, "Sick");
    }

    @Override
    public void play(CombatInterface ci) {
        ci.damagePlayer(10);
        ci.damageEnemy(ci.currentEnemy(), 20);
    }

    @Override
    public String description() {
        return "Deal 20 damage. Receive 8 damage.";
    }

}
