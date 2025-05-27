package inf112.game.deck.cards;

import java.util.Random;

import inf112.game.deck.ACard;
import inf112.game.interfaces.CombatInterface;
import inf112.gfx.Sprite;

/**
 * This card costs 2 energy, and deals 20 damage with 50 % chance of success.
 */
public class RunTestCard extends ACard {

    public static final int COST = 2;

    public RunTestCard() {
        super(COST, 3, Sprite.RUN_TEST_CARD, "Run Tests");
    }

    Random random = new Random();

    @Override
    public void play(CombatInterface ci) {

        int DAMAGE = 20;
        if (random.nextInt(2) == 0)
            ci.damageEnemy(ci.currentEnemy(), DAMAGE);
    }

    @Override
    public String description() {
        return "50% chance to deal 20 damage.";
    }
}
