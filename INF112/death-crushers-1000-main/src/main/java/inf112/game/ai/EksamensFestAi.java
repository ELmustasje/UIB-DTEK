package inf112.game.ai;

import inf112.game.deck.cards.BeerCard;
import inf112.game.interfaces.CombatInterface;

public class EksamensFestAi extends EnemyAI {
    @Override
    protected void executeMove(CombatInterface ci, Intent intent) {
        if (intent.type() == IntentType.SHIELD) {
            ci.shieldEnemy(ci.currentEnemy(), intent.value());
        }
        if (intent.type() == IntentType.ATTACK) {
            ci.damagePlayer(intent.value());
        }

        ci.giveCardToPlayerHandNextTurn(new BeerCard());
    }

    @Override
    public Intent prepareNextMove() {
        return random.nextInt(10) > 5 ? new Intent(IntentType.SHIELD, random.nextInt(5, 15))
                : new Intent(IntentType.ATTACK, random.nextInt(8, 20));
    }
}
