package inf112.game.interfaces;

import inf112.game.components.AStatus;
import inf112.game.deck.ACard;
import inf112.game.entity.AEntity;
import inf112.game.entity.Enemy;
import inf112.game.entity.Player;

import java.util.List;

/**
 * The CombatInterface defines the interaction layer for combat-related actions.
 * It allows enemies to perform attacks, buffs, and other combat actions without
 * directly modifying game logic. This ensures modularity and separation of
 * concerns.
 */
public interface CombatInterface {
    /**
     * Deals damage to the player.
     * 
     * @param amount The amount of damage to deal.
     */
    void damagePlayer(int amount);

    /**
     * Deal damage to an enemy.
     * 
     * @param enemy  enemy to damage
     * @param amount amount of damage
     */
    void damageEnemy(Enemy enemy, int amount);

    /**
     * Applies a shield to the specified enemy.
     * 
     * @param enemy  The enemy receiving the shield.
     * @param amount The amount of shield to apply.
     */
    void shieldEnemy(Enemy enemy, int amount);

    /**
     * Apply shield to player.
     * 
     * @param amount amount of shield to apply
     */
    void shieldPlayer(int amount);

    /**
     * Gives a card to the player.
     * 
     * @param card The card being given to the player.
     */
    void giveCardToPlayerHandNextTurn(ACard card);

    /**
     * Get player instance
     * 
     * @return player
     */
    Player player();

    /**
     * Get a reference to the currently attacking enemy.
     * 
     * @return "this" enemy
     */
    Enemy currentEnemy();

    /**
     * Get targeted enemy. This is the enemy the current playing card should affect.
     * 
     * @return enemy to affect
     */
    Enemy targetEnemy();

    /**
     * Get list of all living enemies in combat.
     * 
     * @return list of all living enemies
     */
    List<Enemy> enemies();

    /**
     * Apply status effect to entity.
     * 
     * @param entity entity to apply component to
     * @param status status to apply
     */
    void applyStatus(AEntity entity, AStatus status);

    /**
     * Gives n number of cards from deck to hand pile
     * 
     * @param n number of cards to give
     */
    void giveRandomCards(int n);

    /**
     * Add energy to player.
     * 
     * @param amount amount of energy to add
     */
    void addEnergy(int amount);

    /**
     * Discard n random cards from hand.
     * 
     * @param n
     */
    void discardNCards(int n);

    /**
     * Give player n gold
     * 
     * @param count
     */
    void giveGold(int n);

}
