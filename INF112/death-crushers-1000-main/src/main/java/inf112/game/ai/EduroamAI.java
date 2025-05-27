package inf112.game.ai;

import inf112.game.deck.ACard;
import inf112.game.deck.cards.LectureCard;
import inf112.game.deck.cards.NoSignalCard;
import inf112.game.deck.cards.RageCard;
import inf112.game.interfaces.CombatInterface;

public class EduroamAI extends EnemyAI {

    private boolean firstTurn = true;
    private int phase = 1;

    @Override
    protected void executeMove(CombatInterface ci, Intent intent) {
        if (firstTurn) {
            ACard noSignal = new NoSignalCard();
            noSignal.setGiven();
            ci.giveCardToPlayerHandNextTurn(noSignal);
            firstTurn = false;
        }

        if (intent.type() == IntentType.ATTACK)
            ci.damagePlayer(intent.value());
        else if (intent.type() == IntentType.SHIELD)
            ci.shieldEnemy(ci.currentEnemy(), intent.value());
        else if (intent.type() == IntentType.UNKNOWN) {
            if (random.nextBoolean()) {
                ACard c = new RageCard();
                c.setGiven();
                ci.giveCardToPlayerHandNextTurn(c);
            } else {
                ACard c = new LectureCard();
                c.setGiven();
                ci.giveCardToPlayerHandNextTurn(c);
            }
        }

        int health = ci.currentEnemy().health();
        int maxHealth = ci.currentEnemy().maxHealth();
        if (health < maxHealth / 2) {
            phase = 2;
        }
        if (health < maxHealth / 4) {
            phase = 3;
        }
    }

    @Override
    public Intent prepareNextMove() {
        if (phase == 0)
            phase = 1;

        return switch (phase) {
            case 1 -> random.nextBoolean() ? new Intent(IntentType.ATTACK, random.nextInt(5, 10))
                    : new Intent(IntentType.SHIELD, random.nextInt(5, 10));

            case 2 -> switch (random.nextInt(3)) {
                case 0 -> new Intent(IntentType.ATTACK, random.nextInt(8, 15));
                case 1 -> new Intent(IntentType.SHIELD, random.nextInt(5, 10));
                case 2 -> new Intent(IntentType.UNKNOWN, 0);

                // Unreachable
                default -> throw new RuntimeException();
            };

            case 3 -> random.nextBoolean() ? new Intent(IntentType.ATTACK, random.nextInt(8, 20))
                    : new Intent(IntentType.SHIELD, random.nextInt(5, 15));

            // Unreachable
            default -> throw new RuntimeException();
        };
    }
}
