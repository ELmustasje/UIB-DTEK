package inf112.game.deck.cards;

import inf112.game.deck.ACard;
import inf112.game.interfaces.CombatInterface;
import inf112.gfx.Sprite;

/**
 * This cards costs 1 energy, but lets the player hold a card for the next
 * round.
 */
public class ProcrastinationCard extends ACard {

    public static final int COST = 1;

    public ProcrastinationCard() {
        super(COST, 4, Sprite.PROCRASTINATION_CARD, "Procrastinate");
    }

    @Override
    public void play(CombatInterface ci) {
        ci.shieldPlayer(8);
        ci.giveRandomCards(1);
    }

    @Override
    public String description() {
        return "Gain 8 shield. Draw 1 card.";
    }
}
