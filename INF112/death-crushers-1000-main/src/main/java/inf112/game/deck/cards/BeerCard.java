package inf112.game.deck.cards;

import inf112.game.deck.ACard;
import inf112.game.interfaces.CombatInterface;
import inf112.gfx.Sprite;

public class BeerCard extends ACard {

    public static final int COST = 1;

    public BeerCard() {
        super(COST, 4, Sprite.BEER_CARD, "Beer");
    }

    @Override
    public void play(CombatInterface ci) {
        ci.player().applyDamageModifier(0.5f);
    }

    @Override
    public String description() {
        return "+50% damage this turn.";
    }

}
