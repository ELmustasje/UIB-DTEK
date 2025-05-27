
package inf112.game.ai;

import inf112.game.components.StunStatus;
import inf112.game.interfaces.CombatInterface;

public class StunnerAI extends EnemyAI {

    @Override
    protected void executeMove(CombatInterface ci, Intent intent) {
        if (intent.type() == IntentType.ATTACK) {
            ci.damagePlayer(intent.value());

            if (calculateShortCircuit()) {
                ci.applyStatus(ci.player(), new StunStatus());
            }
        }
    }

    @Override
    public Intent prepareNextMove() {
        return new Intent(IntentType.ATTACK, calculateAttackDamage());
    }

    private int calculateAttackDamage() {
        return random.nextInt(6, 15);
    }

    boolean calculateShortCircuit() {
        int randomNumber = random.nextInt(100);
        int chanceOfShortCircuit = 20;
        return randomNumber < chanceOfShortCircuit;
    }
}
