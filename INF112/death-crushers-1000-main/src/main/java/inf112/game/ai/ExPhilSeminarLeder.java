package inf112.game.ai;

import inf112.game.components.StunStatus;
import inf112.game.deck.cards.LectureCard;
import inf112.game.interfaces.CombatInterface;

public class ExPhilSeminarLeder extends EnemyAI {
    @Override
    protected void executeMove(CombatInterface ci, Intent intent) {
        if (intent.type() == IntentType.ATTACK) {
            ci.damagePlayer(intent.value());
            return;
        }
        if (intent.type() == IntentType.DEBUFF) {
            int action = random.nextInt(5);
            if (action < 3) {
                ci.giveCardToPlayerHandNextTurn(new LectureCard());
            } else {
                ci.applyStatus(ci.player(), new StunStatus());
            }
        }
    }

    @Override
    public Intent prepareNextMove() {
        return random.nextInt(2) == 1 ? new Intent(IntentType.ATTACK, random.nextInt(9, 16))
                : new Intent(IntentType.DEBUFF, 0);
    }
}
