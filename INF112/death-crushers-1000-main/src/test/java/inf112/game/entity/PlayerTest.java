package inf112.game.entity;

import inf112.game.ai.AttackerAI;
import inf112.game.components.AStatus;
import inf112.gfx.Sprite;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

class PlayerTest {

    private Player player;
    private Enemy enemy;

    @BeforeEach
    void setUp() {
        player = new Player(100, 20, "TestPlayer");
        enemy = new Enemy(50, 15, "TestEnemy", new AttackerAI(), Sprite.TEST);
    }

    @Test
    void testPlayerInitialization() {
        assertEquals(100, player.health());
        assertEquals(20, player.shield());
        assertEquals("TestPlayer", player.getName());
        assertTrue(player.statuses().isEmpty());
        assertTrue(player.components().isEmpty());
    }

    @Test
    void testPlayerAttackEnemyWithoutShield() {
        enemy = new Enemy(60, 0, "Placeholder", new AttackerAI(), Sprite.TEST);
        player.attack(enemy, 30);
        assertEquals(30, enemy.health()); // Enemy should take full damage
    }

    @Test
    void testPlayerAttackEnemyWithShield() {
        player.attack(enemy, 15);
        assertEquals(50, enemy.health()); // Health should remain the same
        assertEquals(0, enemy.shield()); // Shield should absorb all damage
    }

    @Test
    void testPlayerAttackEnemyShieldBreak() {
        player.attack(enemy, 25);
        assertEquals(40, enemy.health()); // 10 absorbed by shield, 15 goes to health
        assertEquals(0, enemy.shield());
    }

    @Test
    void testAddShield() {
        player.addShield(10);
        assertEquals(30, player.shield()); // Shield should increase
    }

    @Test
    void testShieldAbsorbsDamage() {
        player.attack(player, 15);
        assertEquals(5, player.shield()); // 15 damage absorbed, 5 shield remains
        assertEquals(100, player.health()); // Health is unaffected
    }

    @Test
    void testShieldBreaksAndHealthTakesDamage() {
        player.attack(player, 30);
        assertEquals(0, player.shield()); // Shield fully depleted
        assertEquals(90, player.health()); // Remaining 10 damage hits health
    }

    @Test
    void testStartTurnAppliesStatuses() {
        AStatus mockStatus1 = Mockito.mock(AStatus.class);
        AStatus mockStatus2 = Mockito.mock(AStatus.class);

        List<AStatus> statuses = new ArrayList<>();
        statuses.add(mockStatus1);
        statuses.add(mockStatus2);

        Mockito.doNothing().when(mockStatus1).apply(player);
        Mockito.doNothing().when(mockStatus2).apply(player);

        player.statuses().addAll(statuses);
        player.startTurn();

        Mockito.verify(mockStatus1, Mockito.times(1)).apply(player);
        Mockito.verify(mockStatus2, Mockito.times(1)).apply(player);
    }

    @Test
    void testStartTurnDoesNotThrowException() {
        assertDoesNotThrow(() -> player.startTurn());
    }

    @Test
    void testEndTurnDoesNotThrowException() {
        assertDoesNotThrow(() -> player.endRound());
    }

    @Test
    void testPlayerDiesWhenHealthReachesZero() {
        player.dealDamage(120); // 100 hp + 20 shield
        assertEquals(0, player.health());
        assertEquals(true, player.isDead());
    }

    @Test
    void testNegativeDamageHasNoEffect() {
        player.dealDamage(-10);
        assertEquals(100, player.health());
        assertEquals(20, player.shield());
    }

    @Test
    void testRemoveStatusEffect() {
        AStatus mockStatus = Mockito.mock(AStatus.class);
        player.addStatus(mockStatus);
        assertEquals(1, player.statuses().size());

        player.statuses().remove(mockStatus);
        assertEquals(0, player.statuses().size());
    }

    @Test
    void testDeadPlayerCannotAttack() {
        player.dealDamage(120);
        player.attack(enemy, 10); // Should have no effect
        assertEquals(50, enemy.health()); // Enemy should remain unaffected
    }

    @Test
    void testDeadPlayerCannotHeal() {
        player.dealDamage(120);
        player.heal(50); // Should have no effect if player is "dead"
        assertEquals(0, player.health());
    }

    @Test
    void testAddingNegativeShieldDoesNothing() {
        player.addShield(-10);
        assertEquals(20, player.shield()); // Shield should not decrease
    }

    @Test
    void testOverhealingDoesNotExceedMaxHealth() {
        player.heal(50); // Healing should not go beyond 100
        assertEquals(100, player.health()); // Health should remain capped
    }

    @Test
    void testHealingWorksNormally() {
        player.dealDamage(50); // Reduce HP from 100 to 70 and remove shield
        player.heal(20); // Heal back to 90
        assertEquals(90, player.health());
    }

    @Test
    void testHealingNegativeDoesNothing() {
        player.dealDamage(50);
        player.heal(-10);
        assertEquals(70, player.health()); // Should remain unchanged
    }

    @Test
    void testDeadPlayerCannotBeHealed() {
        player.dealDamage(200); // Kill the player
        player.heal(50);
        assertEquals(0, player.health()); // Should still be 0
    }

    @Test
    void testAddGoldPositive() {
        player.addGold(100);
        assertEquals(100, player.getGold(), "Gold should increase by the added positive amount.");
    }

    @Test
    void testAddGoldNegativeDoesNothing() {
        player.addGold(-50);
        assertEquals(0, player.getGold(), "Adding negative gold should have no effect.");
    }

    @Test
    void testPurchaseSuccessful() {
        player.addGold(100);
        boolean purchaseResult = player.purchase(80);
        assertTrue(purchaseResult, "Purchase should succeed when enough gold is available.");
        assertEquals(20, player.getGold(), "Gold should be reduced by the purchase amount.");
    }

    @Test
    void testPurchaseUnsuccessful() {
        player.addGold(50);
        boolean purchaseResult = player.purchase(80);
        assertFalse(purchaseResult, "Purchase should fail when insufficient gold is available.");
        assertEquals(50, player.getGold(), "Gold should remain unchanged if purchase fails.");
    }

    @Test
    void testDefaultConstructorForStartingEnergy() {
        // Using the constructor that does not supply starting energy, defaulting to 3.
        Player defaultEnergyPlayer = new Player(100, 20, "DefaultTest");
        assertEquals(3, defaultEnergyPlayer.energy(), "Default energy should be 3.");
        assertEquals(3, defaultEnergyPlayer.startEnergy(), "Default starting energy should be 3.");
    }

    @Test
    void testGetEnergyAndStartingEnergy() {
        // For the player created in your setUp (using Player(100, 20, "TestPlayer")),
        // starting energy defaults to 3.
        assertEquals(3, player.energy(), "Energy getter should return the correct energy value.");
        assertEquals(3, player.startEnergy(),
                "Starting energy getter should return the correct value.");
    }

    @Test
    void testAttackWithNullTarget() {
        // Attack with a null target should do nothing and not throw an exception.
        assertDoesNotThrow(() -> player.attack(null, 10),
                "Attacking a null target should not throw an exception.");
    }
}
