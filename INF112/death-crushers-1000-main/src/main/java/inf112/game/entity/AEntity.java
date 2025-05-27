package inf112.game.entity;

import inf112.game.components.AStatus;
import inf112.game.components.Component;

import java.util.ArrayList;
import java.util.List;

/**
 * Represents a base entity in the game, which can be a for example players or
 * enemies.
 * Entities have health, shields, and components that define their abilities.
 * They can take damage, be healed, and maintain a list of status effects.
 * This class follows the principles of an Entity-Component-System (ECS).
 */
public abstract class AEntity {
    protected String name;
    protected int health;
    protected int shield;
    protected int maxHealth;
    protected boolean isDead;
    protected List<Component> components;
    protected List<AStatus> statuses;

    protected float damageModifier;
    protected float shieldModifier;

    /**
     * Constructs an Entity with specified health, shield, and name.
     *
     * @param health The initial health of the entity.
     * @param shield The initial shield value.
     * @param name   The name of the entity.
     */
    AEntity(int health, int shield, String name) {
        this.name = name;
        this.health = health;
        this.maxHealth = health;
        this.shield = shield;
        this.components = new ArrayList<>();
        this.statuses = new ArrayList<>();
        this.isDead = false;

        resetAllModifiers();
    }

    /**
     * Adds a component to the entity.
     *
     * @param component The component to be added.
     */
    public void addComponent(Component component) {
        components.add(component);
    }

    /**
     * Adds a status effect to the entity.
     *
     * @param status The status effect to be added.
     */
    public void addStatus(AStatus status) {
        statuses.add(status);
    }

    /**
     * Actually applies the effect of the status and decreases its duration. If it
     * has run out it is removed from the list at
     * 
     * @param status
     */
    public void applyStatus(AStatus status) {
        status.apply(this);
        status.decreaseRounds();
    }

    public void purgeStatuses() {
        statuses.removeIf(status -> status.rounds() == 0);
    }

    /**
     * Increases the entity's shield by the specified amount.
     * Shield cannot be increased if the entity is dead.
     *
     * @param amount The amount of shield to be added.
     */
    public void addShield(int amount) {
        amount += (int) (amount * shieldModifier);

        if (amount > 0 && !isDead)
            shield += amount;
    }

    /**
     * Applies damage to the entity, reducing shield first, then health.
     * If damage exceeds total health and shield, the entity dies.
     *
     * @param amount The amount of damage to be dealt.
     */
    public void dealDamage(int amount) {
        if (amount < 0 || isDead)
            return; // Ignore negative damage and dead entities

        int totalHealth = shield + health;
        if (amount >= totalHealth) { // Instantly kill if damage exceeds total health
            shield = 0;
            health = 0;
            isDead = true;
            return;
        }

        // Damage shield first
        int remainingDamage = Math.max(0, amount - shield);
        shield = Math.max(0, shield - amount);

        // Damage health with any remaining damage
        health -= remainingDamage;

        // If health drops to 0, mark as dead
        if (health <= 0)
            isDead = true;
    }

    /**
     * Attacks enemy or player, dealing damage. Applies modifiers if any.
     *
     * @param target The entity being attacked.
     * @param amount The amount of damage to deal.
     */
    public void attack(AEntity target, int amount) {
        if (target == null)
            return;

        amount += (int) (amount * damageModifier);
        target.dealDamage(amount);
    }

    /**
     * Removes a component from the entity.
     *
     * @param component The component to be removed.
     */
    public void removeComponent(Component component) {
        components.remove(component);
    }

    /**
     * Retrieves the list of components attached to the entity.
     *
     * @return A list of the entity's components.
     */
    public List<Component> components() {
        return components;
    }

    /**
     * Retrieves the list of status effects currently affecting the entity.
     *
     * @return A list of the entity's status effects.
     */
    public List<AStatus> statuses() {
        return statuses;
    }

    /**
     * Retrieves the entity's current health.
     *
     * @return The entity's health value.
     */
    public int health() {
        return health;
    }

    /**
     * Retrieves the entity's max health.
     *
     * @return The entity's max health value.
     */
    public int maxHealth() {
        return maxHealth;
    }

    /**
     * Retrieves the entity's current shield value.
     *
     * @return The entity's shield value.
     */
    public int shield() {
        return shield;
    }

    /**
     * Retrieves the entity's name.
     *
     * @return The entity's name.
     */
    public String getName() {
        return name;
    }

    /**
     * Updates the entity's name.
     *
     * @param name The new name to assign.
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Checks if the entity is dead.
     *
     * @return true if the entity is dead, false otherwise.
     */
    public boolean isDead() {
        return isDead;
    }

    /**
     * Heals the entity by a specified amount, up to its max health.
     * Healing has no effect if the entity is dead.
     *
     * @param amount The amount of health to restore.
     */
    public void heal(int amount) {
        if (!isDead && amount > 0) {
            health = Math.min(health + amount, maxHealth);
        }
    }

    /**
     * Adds a damage modifier (multiplier) to entity, reducing or increasing damage
     * given by given percentage. Does not stack.
     * Expressed as a floating number (25% = 0.25, -10% = -0.10)
     * 
     * @param percentage
     */
    public void applyDamageModifier(float percentage) {
        damageModifier += percentage;
    }


    /**
     * Resets all damage and shield modifiers. Should be called at end of round when
     * all effects have been applied and played out.
     */
    public void resetAllModifiers() {
        damageModifier = 0;
        shieldModifier = 0;
    }
}
