package inf112.game.deck.cards;

import inf112.game.components.StunStatus;
import inf112.game.deck.ACard;
import inf112.game.interfaces.CombatInterface;
import inf112.gfx.Sprite;

public class StunCard extends ACard {

    public static final int COST = 2;

    public StunCard() {
        super(COST, 2, Sprite.STUN_CARD, "Stun");
    }

    @Override
    public void play(CombatInterface ci) {
        ci.applyStatus(ci.targetEnemy(), new StunStatus());
    }

    @Override
    public String description() {
        return "Apply stun. Deals 7 damage 2 turns.";
    }
}
