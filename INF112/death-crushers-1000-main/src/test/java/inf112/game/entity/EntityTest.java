package inf112.game.entity;

import inf112.game.components.AStatus;
import inf112.game.components.Component;
import inf112.game.components.ShieldComponent;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

public class EntityTest {

    private TestEntity entity;

    class TestEntity extends AEntity {
        public TestEntity(int health, int shield) {
            super(health, shield, "Placeholder");
        }
    }

    class DummyStatus extends AStatus {

        public DummyStatus(Integer rounds) {
            super(rounds);
        }

        @Override
        public void apply(AEntity entity) {
        }
    }

    @BeforeEach
    void setUp() {
        entity = new TestEntity(100, 50);
    }

    @Test
    void testInitialValues() {
        assertEquals(100, entity.health(), "Initial health should be 100");
        assertEquals(50, entity.shield(), "Initial shield should be 50");
    }

    @Test
    void testAddComponent() {
        ShieldComponent comp = new ShieldComponent(5);
        entity.addComponent(comp);
        List<Component> comps = entity.components();
        assertEquals(1, comps.size(), "Component list should have one element after adding one component");
        assertTrue(comps.contains(comp), "Component list should contain the added component");
    }

    @Test
    void testRemoveComponent() {
        ShieldComponent comp = new ShieldComponent(5);
        entity.addComponent(comp);
        assertTrue(entity.components().contains(comp));
        entity.removeComponent(comp);
        assertFalse(entity.components().contains(comp), "Component list should not contain the removed component");
    }

    @Test
    void testAddStatus() {
        DummyStatus status = new DummyStatus(2);
        entity.addStatus(status);
        List<AStatus> statuses = entity.statuses();
        assertEquals(1, statuses.size(), "Status list should have one element after adding one status");
        assertTrue(statuses.contains(status), "Status list should contain the added status");
    }

    @Test
    void testAddShield() {
        int initialShield = entity.shield();
        int addedShield = 20;
        int expectedShield = initialShield + addedShield;

        // add shield
        entity.addShield(20);
        assertEquals(expectedShield, entity.shield(), "Shield should increase by added amount");

        // invalid shield
        entity.addShield(-20);
        assertEquals(expectedShield, entity.shield(), "Shield should not have changed if added shield is negative");

        // entity is dead
        entity.isDead = true;
        entity.addShield(addedShield);
        assertEquals(expectedShield, entity.shield(), "Shield should not have changed if entity is dead");
    }

    @Test
    void testDealDamage() {

        // enough shield to absorb all damage
        entity.dealDamage(30);
        assertEquals(20, entity.shield(), "Shield should be reduced by damage when damage is less than shield");
        assertEquals(100, entity.health(), "Health should remain unchanged if shield absorbs all damage");

        // not enough shield to absorb all damage
        entity.dealDamage(30);
        assertEquals(0, entity.shield(), "Shield should be 0 since damage exceeds shield");
        assertEquals(90, entity.health(), "Health should be reduced by remaining damage after shield is depleted");

        // no shield
        entity = new TestEntity(100, 0);
        entity.dealDamage(40);
        assertEquals(60, entity.health(), "Health should be reduced by full damage when there is no shield");

        entity.dealDamage(-10);
        assertEquals(60, entity.health(), "Health should not change when damage is negative");

        // entity is dead
        assertFalse(entity.isDead, "Entity should not be dead when health is above 0");
        entity.dealDamage(60);
        assertTrue(entity.isDead, "Entity should be dead when health reaches 0");

        entity.dealDamage(10);
        assertEquals(0, entity.health(), "Health should not change when entity is dead");
    }
}
