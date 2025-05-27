package inf112.game.deck.cards;

import inf112.game.deck.ACard;
import inf112.game.interfaces.CombatInterface;
import inf112.gfx.Sprite;

public class RageCard extends ACard {

    public static final int COST = 0;

    public RageCard() {
        super(COST, 3, Sprite.RAGE_CARD, "Rage");
    }

    @Override
    public void play(CombatInterface ci) {
        ci.damageEnemy(ci.currentEnemy(), 5);

        ACard rage = new RageCard();
        rage.setGiven();
        ci.giveCardToPlayerHandNextTurn(rage);
    }

    @Override
    public String description() {
        return "Deal 5 damage. Add a Rage to your draw pile.";
    }

}
