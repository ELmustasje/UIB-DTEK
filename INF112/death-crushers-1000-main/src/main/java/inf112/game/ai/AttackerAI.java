package inf112.game.ai;

import inf112.game.interfaces.CombatInterface;

public class AttackerAI extends EnemyAI {

    @Override
    protected void executeMove(CombatInterface ci, Intent intent) {
        if (intent.type() == IntentType.ATTACK) {
            ci.damagePlayer(intent.value());
        }
    }

    @Override
    public Intent prepareNextMove() {
        return new Intent(IntentType.ATTACK, calculateAttackDamage());
    }

    private int calculateAttackDamage() {
        return random.nextInt(6, 15);
    }
}
