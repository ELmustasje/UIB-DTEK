package inf112.utils;

import com.badlogic.gdx.utils.GdxRuntimeException;
import com.badlogic.gdx.utils.NumberUtils;

import inf112.core.Config;

import java.io.Serializable;

/**
 * This class is a custom implementation of the Vector2 class from the libGDX
 * library.
 * Unlike the original libGDX Vector2 class, this implementation ensures that
 * all methods
 * return a new Vector2 instance instead of modifying the original vector. This
 * approach
 * makes the class immutable in behavior, allowing for safe usage without
 * unintended side effects.
 * <p>
 * Benefits:
 * - Prevents unintended side effects when vectors are passed to multiple
 * functions that
 * may internally modify the vector, ensuring code correctness and avoiding
 * bugs.
 * <p>
 * Drawback:
 * - Introduces some overhead due to the creation of new Vector2 instances for
 * each operation.
 */

public class Vec2 implements Serializable {
    public float x;
    public float y;

    public Vec2() {
    }

    public Vec2(float x, float y) {
        this.x = x;
        this.y = y;
    }

    public Vec2(Vec2 v) {
        this.set(v);
    }

    /**
     * Get a vector with relative position to screen width/height.
     * 
     * @param x float from 0 to 1
     * @param y float from 0 to 1
     * @return
     */
    public static Vec2 relative(float x, float y) {
        return new Vec2(Config.VIEW_WIDTH * x, Config.VIEW_HEIGHT * y);
    }

    /**
     * Returns a new vector accelerated towards the target. Acceleration is relative
     * to distance to target and given speed parameter.
     * 
     * @param pos    Original position
     * @param target Target position to reach
     * @param speed  A positive value. Default is 1.0.
     * @return New position vector. Does not modify original.
     */
    public static Vec2 accelerateTowards(Vec2 pos, Vec2 target, float speed) {
        final float limit = 0.5f;

        float dist = pos.dst(target);
        float trueSpeed = dist * 0.1f * speed;

        // Delta time
        // trueSpeed *= Gdx.graphics.getDeltaTime() / (1f / Config.FPS) ;

        if (dist < limit)
            return target.cpy();

        Vec2 direction = target.sub(pos).nor();
        return pos.add(direction.scl(trueSpeed));
    }

    /**
     * Accelerates this vector towards the target and returns true when reached.
     * This is a shorthand for setting the vector position to the calculated
     * accelerated vector.
     * 
     * @param target target to reach
     * @param speed  speed modifier (1 is default)
     * @return true when at target
     */
    public boolean accelerateTowards(Vec2 target, float speed) {
        this.set(Vec2.accelerateTowards(this, target, speed));
        return epsilonEquals(target);
    }

    public boolean epsilonEquals(Vec2 other, float epsilon) {
        if (other == null) {
            return false;
        } else if (Math.abs(other.x - this.x) > epsilon) {
            return false;
        } else {
            return !(Math.abs(other.y - this.y) > epsilon);
        }
    }

    public boolean epsilonEquals(Vec2 other) {
        return this.epsilonEquals(other, 1.0E-6F);
    }

    public Vec2 cpy() {
        return new Vec2(this);
    }

    public float len() {
        return (float) Math.sqrt((this.x * this.x + this.y * this.y));
    }

    public Vec2 set(Vec2 v) {
        this.x = v.x;
        this.y = v.y;
        return this;
    }

    public Vec2 set(float x, float y) {
        this.x = x;
        this.y = y;
        return this;
    }

    // change

    /**
     * Subtracts the given vector from this vector and returns a new Vector2
     * instance.
     * The original vector remains unchanged.
     */
    public Vec2 sub(Vec2 v) {
        return new Vec2(this.x - v.x, this.y - v.y);
    }

    // change
    public Vec2 sub(float x, float y) {
        return new Vec2(this.x - x, this.y - y);
    }

    // change

    /**
     * Normalizes this vector and returns a new Vector2 instance with a length of 1.
     * The original vector remains unchanged.
     */
    public Vec2 nor() {
        float len = this.len();
        if (len != 0.0F) {
            float x = this.x / len;
            float y = this.y / len;
            return new Vec2(x, y);
        }
        return this;
    }

    // change

    /**
     * Adds the given vector to this vector and returns a new Vector2 instance.
     * The original vector remains unchanged.
     */
    public Vec2 add(Vec2 v) {
        return new Vec2(this.x + v.x, this.y + v.y);
    }

    // change
    public Vec2 add(float x, float y) {
        return new Vec2(this.x + x, this.y + y);
    }

    // change

    /**
     * Scales this vector by the given scalar and returns a new Vector2 instance.
     * The original vector remains unchanged.
     */
    public Vec2 scl(float scalar) {
        return new Vec2(this.x * scalar, this.y * scalar);
    }

    public float dst(Vec2 v) {
        float x_d = v.x - this.x;
        float y_d = v.y - this.y;
        return (float) Math.sqrt((x_d * x_d + y_d * y_d));
    }

    public String toString() {
        return "(" + this.x + "," + this.y + ")";
    }

    public Vec2 fromString(String v) {
        int s = v.indexOf(44, 1);
        if (s != -1 && v.charAt(0) == '(' && v.charAt(v.length() - 1) == ')') {
            try {
                float x = Float.parseFloat(v.substring(1, s));
                float y = Float.parseFloat(v.substring(s + 1, v.length() - 1));
                return this.set(x, y);
            } catch (NumberFormatException ignored) {
            }
        }

        throw new GdxRuntimeException("Malformed Vector2: " + v);
    }

    public int hashCode() {
        int result = 1;
        result = 31 * result + NumberUtils.floatToIntBits(this.x);
        result = 31 * result + NumberUtils.floatToIntBits(this.y);
        return result;
    }

    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        } else if (obj == null) {
            return false;
        } else if (this.getClass() != obj.getClass()) {
            return false;
        } else {
            Vec2 other = (Vec2) obj;
            if (NumberUtils.floatToIntBits(this.x) != NumberUtils.floatToIntBits(other.x)) {
                return false;
            } else {
                return NumberUtils.floatToIntBits(this.y) == NumberUtils.floatToIntBits(other.y);
            }
        }
    }

    public Vec2 setZero() {
        this.x = 0.0F;
        this.y = 0.0F;
        return this;
    }

}
