package inf112.game.deck.cards;

import inf112.game.deck.ACard;
import inf112.game.interfaces.CombatInterface;
import inf112.gfx.Sprite;

public class LectureCard extends ACard {

    public static int COST = 0;

    public LectureCard() {
        super(COST, 3, Sprite.LECTURE_CARD, "Lecture");
        playable = false;
    }

    @Override
    public void play(CombatInterface ci) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'play'");
    }

    @Override
    public String description() {
        return "Just a boring lecture. Unplayable.";
    }

    @Override
    public boolean isDebuff() {
        return true;
    }

    @Override
    public boolean unplayable() {
        return true;
    }

}
