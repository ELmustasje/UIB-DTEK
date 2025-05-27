package inf112.game.entity;

import inf112.game.ai.EnemyAI;
import inf112.game.ai.Intent;
import inf112.game.interfaces.CombatInterface;
import inf112.gfx.Atlas;
import inf112.gfx.Drawable;
import inf112.gfx.DrawableType;
import inf112.gfx.Sprite;
import inf112.utils.Vec2;

/**
 * Represents an AI-controlled enemy in combat.
 * Enemies execute predefined actions using {@link EnemyAI} and follow an
 * intent-based
 * turn system where they prepare their next move in advance.
 */
public class Enemy extends AEntity implements Drawable {

    private final EnemyAI ai;
    private final Sprite sprite;
    private final Vec2 pos;
    private final DrawableType TYPE = DrawableType.SPRITE;

    /**
     * Constructs an Enemy with predefined health, shield, and type.
     *
     * @param health    The enemy's initial health.
     * @param shield    The enemy's initial shield value.
     * @param name      The name of the enemy.
     * @param enemyType The type of enemy (defines behavior).
     */
    public Enemy(int health, int shield, String name, EnemyAI ai, Sprite sprite) {
        super(health, shield, name);
        this.ai = ai;
        this.sprite = sprite;
        this.pos = new Vec2().setZero();
    }

    /**
     * Executes the enemy's turn by calling its AI.
     *
     * @param ci The combat interface that handles interactions.
     */
    public void takeTurn(CombatInterface ci) {
        ai.turn(ci);
    }

    public void reset() {
        shield = 0;
        resetAllModifiers();
    }

    /**
     * Returns the enemy's next move.
     * 
     * @return The move type the enemy will perform next.
     */
    public Intent intent() {
        return ai.intent();
    }

    /**
     * Retrieves the AI instance governing this enemy.
     * 
     * @return The enemy's AI instance.
     */
    public EnemyAI ai() {
        return ai;
    }

    @Override
    public Sprite sprite() {
        return sprite;
    }

    @Override
    public Vec2 pos() {
        return pos;
    }

    @Override
    public float width() {
        return Atlas.sizeOf(sprite).x;
    }

    @Override
    public float height() {
        return Atlas.sizeOf(sprite).y;
    }

    @Override
    public String text() {
        return name;
    }

    @Override
    public DrawableType type() {
        return TYPE;
    }
}
