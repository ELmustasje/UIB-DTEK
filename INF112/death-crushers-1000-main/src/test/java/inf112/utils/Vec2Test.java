package inf112.utils;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class Vec2Test {

    private Vec2 v1;
    private Vec2 v2;

    @BeforeEach
    public void setUp() {
        v1 = new Vec2(3, 4);
        v2 = new Vec2(1, 2);
    }

    @Test
    public void notChanged() {
        assertEquals(3, v1.x);
        assertEquals(4, v1.y);
    }

    @Test
    public void testDefaultConstructor() {
        Vec2 defaultVector = new Vec2();
        assertEquals(0, defaultVector.x);
        assertEquals(0, defaultVector.y);
    }

    @Test
    public void testParameterizedConstructor() {
        assertEquals(3, v1.x);
        assertEquals(4, v1.y);
    }

    @Test
    public void testAdd() {
        Vec2 result = v1.add(1, 2);
        assertEquals(4, result.x);
        assertEquals(6, result.y);
        notChanged();

        Vec2 result2 = v2.add(3, 5);
        assertEquals(4, result2.x);
        assertEquals(7, result2.y);
    }

    @Test
    public void testSub() {
        Vec2 result = v1.sub(1, 2);
        assertEquals(2, result.x);
        assertEquals(2, result.y);
        notChanged();

        // Additional test for changed behavior
        Vec2 result2 = v2.sub(2, 1);
        assertEquals(-1, result2.x);
        assertEquals(1, result2.y);
    }

    @Test
    public void testNormalize() {
        Vec2 normalized = v1.nor();
        assertEquals(1, normalized.len(), 0.0001);
        notChanged();

        // Additional test for changed behavior
        Vec2 normalized2 = v2.nor();
        assertEquals(1, normalized2.len(), 0.0001);
        assertEquals(v2.x / v2.len(), normalized2.x, 0.0001);
        assertEquals(v2.y / v2.len(), normalized2.y, 0.0001);
    }

    @Test
    public void testScalarMultiplication() {
        Vec2 result = v1.scl(2);
        assertEquals(6, result.x);
        assertEquals(8, result.y);
        notChanged();

        // Additional test for changed behavior
        Vec2 result2 = v2.scl(3);
        assertEquals(3, result2.x);
        assertEquals(6, result2.y);
    }

    @Test
    public void testLengthOfVector() {
        assertEquals(5, v1.len(), 0.0001);
        assertEquals(Math.sqrt(5), v2.len(), 0.0001);
        notChanged();
    }

    @Test
    public void testConstructorWithValues() {
        Vec2 v = new Vec2(3.0f, 4.0f);
        assertEquals(3.0f, v.x, 0.0f);
        assertEquals(4.0f, v.y, 0.0f);
    }

    @Test
    public void testNor() {
        Vec2 v = new Vec2(3.0f, 4.0f);
        Vec2 result = v.nor();
        assertEquals(0.6f, result.x, 0.01f); // Normalized values
        assertEquals(0.8f, result.y, 0.01f);
    }

    @Test
    public void testLength() {
        Vec2 v = new Vec2(3.0f, 4.0f);
        assertEquals(5.0f, v.len(), 0.0f); // sqrt(3^2 + 4^2) = 5
    }

    @Test
    public void testEpsilonEquals() {
        Vec2 v1 = new Vec2(1.0f, 1.0f);
        Vec2 v2 = new Vec2(1.0f, 1.000001f);
        assertTrue(v1.epsilonEquals(v2, 0.00001f));
    }

    @Test
    public void testFromString() {
        Vec2 v = new Vec2().fromString("(3.0,4.0)");
        assertEquals(3.0f, v.x, 0.0f);
        assertEquals(4.0f, v.y, 0.0f);
    }

    @Test
    public void testNegativeVector() {
        Vec2 v = new Vec2(-3.0f, -4.0f);
        assertEquals(-3.0f, v.x, 0.0f);
        assertEquals(-4.0f, v.y, 0.0f);
        assertEquals(5.0f, v.len(), 0.0f); // Length should still be 5 (|(-3, -4)| = 5)
    }

    // Test for NaN values
    @Test
    public void testNaNVector() {
        Vec2 v = new Vec2(Float.NaN, Float.NaN);
        assertTrue(Float.isNaN(v.x));
        assertTrue(Float.isNaN(v.y));
    }

    // Test for Infinity values
    @Test
    public void testInfinityVector() {
        Vec2 v = new Vec2(Float.POSITIVE_INFINITY, Float.POSITIVE_INFINITY);
        assertEquals(Float.POSITIVE_INFINITY, v.x);
        assertEquals(Float.POSITIVE_INFINITY, v.y);
    }

    // Test for zero-length vector
    @Test
    public void testZeroLengthVector() {
        Vec2 v = new Vec2(0.0f, 0.0f);
        assertEquals(0.0f, v.len(), 0.0f);
        Vec2 result = v.nor();
        assertEquals(0.0f, result.x, 0.0f); // Nor of a zero vector is still a zero vector
        assertEquals(0.0f, result.y, 0.0f);
    }

}
