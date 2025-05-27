package inf112.game.entity;

import inf112.game.components.AStatus;

/**
 * Represents the player's character in combat.
 * The Player class extends {@link AEntity} and provides player-specific
 * actions,
 * such as attacking enemies and processing turn-based mechanics.
 */
public class Player extends AEntity {
    private int gold; // Gold to be used in shops. May be moved to a wallet class
    private int energy; // Energy to be spent on cards.
    private final int startingEnergy; // Energy that the player starts with every round

    /**
     * Constructs a Player object with the provided parameters.
     * 
     * @param health         starting health
     * @param shield         starting shield
     * @param name           name
     * @param startingEnergy starting energy
     */
    public Player(int health, int shield, String name, int startingEnergy) {
        // Player name should be obtained from user input
        super(health, shield, name);
        gold = 0;
        energy = startingEnergy;
        this.startingEnergy = startingEnergy;
    }

    public Player(int health, int shield, String name) {
        this(health, shield, name, 3);
    }

    /**
     * Called at the start of the player's turn.
     * This can later include status effect handling.
     */
    public void startTurn() {
        for (AStatus status : statuses()) {
            status.apply(this);
        }
    }

    /**
     * Called at the end of the round. Resets energy and shield.
     */
    public void endRound() {
        resetEnergy();
        resetAllModifiers();
        shield = 0;
    }

    /**
     * Retrieves gold of player
     * 
     * @return int amount of gold
     */
    public int getGold() {
        return gold;
    }

    public void addGold(int n) {
        if (n >= 0)
            gold += n;
    }

    /**
     * Method used to purchase items in shop.
     * If player has enough gold, subtracts price (n) from gold and returns true.
     * Returns false otherwise
     * 
     * @param n price of object
     * @return true if purchase successful.
     */
    public boolean purchase(int n) {
        if (gold >= n) {
            gold -= n;
            return true;
        }
        return false;
    }

    public int energy() {
        return energy;
    }

    public int startEnergy() {
        return startingEnergy;
    }

    /**
     * Subtracts amount from energy level. Does not check if amount is less or equal
     * current energy value.
     * 
     * @param amount amount to use
     */
    public void useEnergy(int amount) {
        energy -= amount;
    }

    public void resetEnergy() {
        energy = startingEnergy;
    }

    public void addEnergy(int amount) {
        energy += amount;
    }
}
