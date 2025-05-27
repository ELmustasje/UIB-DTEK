package inf112.game.ai;

import inf112.game.interfaces.CombatInterface;

import java.util.Random;

/**
 * Handles enemy decision-making and AI logic.
 * EnemyAI determines and executes an enemy's turn based on predefined move
 * types.
 */
public abstract class EnemyAI {
    protected Random random;
    protected Intent intent;

    /**
     * Constructs an AI controller for an Enemy instance.
     */
    protected EnemyAI() {
        this.random = new Random();
        this.intent = prepareNextMove();
    }

    public Intent intent() {
        return intent;
    }

    /**
     * Executes the enemy's turn.
     * The turn consists of a move and preparing the next move.
     *
     * @param ci The combat interface handling the actions.
     */
    public void turn(CombatInterface ci) {
        executeMove(ci, intent);
    }

    public void prepare() {
        this.intent = prepareNextMove();
    }

    /**
     * Exectues the enemy's intended move.
     * 
     * @param ci       The combat interface handling the actions.
     * @param moveType The move type to execute
     */
    protected abstract void executeMove(CombatInterface ci, Intent intent);

    /**
     * Prepares the next move for the enemy.
     *
     * @return The next move type.
     */
    public abstract Intent prepareNextMove();
}
