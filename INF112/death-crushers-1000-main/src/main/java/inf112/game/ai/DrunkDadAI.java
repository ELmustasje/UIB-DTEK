package inf112.game.ai;

import inf112.game.interfaces.CombatInterface;

public class DrunkDadAI extends EnemyAI {

    private int health = 999;
    private int maxHealth = 999;

    public DrunkDadAI() {
    }

    @Override
    protected void executeMove(CombatInterface ci, Intent intent) {
        health = ci.currentEnemy().health();
        maxHealth = ci.currentEnemy().maxHealth();

        if (intent.type() == IntentType.ATTACK) {
            ci.damagePlayer(intent.value());
        } else if (intent.type() == IntentType.SHIELD) {
            ci.shieldEnemy(ci.currentEnemy(), intent.value());
        }
    }

    @Override
    public Intent prepareNextMove() {
        if (health <= 10 && health > 0) {
            return new Intent(IntentType.ATTACK, random.nextInt(30, 35));
        }

        if (health <= maxHealth / 2 && health > 0) {
            return random.nextInt(0, 6) >= 2 ? new Intent(IntentType.SHIELD, random.nextInt(10, 20))
                    : new Intent(IntentType.ATTACK, random.nextInt(5, 15));
        }

        return new Intent(IntentType.ATTACK, random.nextInt(4, 9));
    }
}
