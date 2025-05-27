package inf112.game.deck.cards;

import inf112.game.deck.ACard;
import inf112.game.interfaces.CombatInterface;
import inf112.gfx.Sprite;

/**
 * This card costs 1 energy, and gives the player 10 sheilds.
 */
public class TikTokBreakCard extends ACard {

    public static final int COST = 1;

    public TikTokBreakCard() {
        super(COST, 2, Sprite.TIKTOK_BREAK_CARD, "TikTok Break");
    }

    @Override
    public void play(CombatInterface ci) {
        ci.shieldPlayer(10);
    }

    @Override
    public String description() {
        return "Gain 10 shield.";
    }
}
