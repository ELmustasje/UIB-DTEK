package inf112.game.deck.cards;

import inf112.game.deck.ACard;
import inf112.game.interfaces.CombatInterface;
import inf112.gfx.Sprite;

public class NoSignalCard extends ACard {

    public static final int COST = 0;

    public NoSignalCard() {
        super(COST, 3, Sprite.NO_SIGNAL_CARD, "No Signal");
    }

    @Override
    public void play(CombatInterface ci) {
        ci.damagePlayer(4);
    }

    @Override
    public String description() {
        return "At the end of your turn, take 4 damage. Unplayable.";
    }

    @Override
    public boolean unplayable() {
        return true;
    }

    @Override
    public boolean autoPlay() {
        return true;
    }

    @Override
    public boolean isDebuff() {
        return true;
    }

}
