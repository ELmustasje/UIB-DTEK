package inf112.game.deck.cards;

import inf112.game.deck.ACard;
import inf112.game.interfaces.CombatInterface;
import inf112.gfx.Sprite;

public class SkipLectureCard extends ACard {

    public static int COST = 2;

    public SkipLectureCard() {
        super(COST, 2, Sprite.SKIP_LECTURE_CARD, "Skip Lecture");
    }

    @Override
    public void play(CombatInterface ci) {
        ci.damageEnemy(ci.currentEnemy(), 15);
    }

    @Override
    public String description() {
        return "Deal 15 damage.";
    }
}
