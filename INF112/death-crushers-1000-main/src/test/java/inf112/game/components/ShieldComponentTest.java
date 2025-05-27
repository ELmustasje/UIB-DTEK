package inf112.game.components;

import inf112.game.entity.AEntity;
import org.junit.jupiter.api.Test;

import static org.mockito.Mockito.*;

class ShieldComponentTest {

    @Test
    void testApplyAddsCorrectShieldAmount() {
        AEntity entity = mock(AEntity.class);
        ShieldComponent shield = new ShieldComponent(20);

        shield.apply(entity);

        verify(entity, times(1)).addShield(20);
    }

    @Test
    void testApplyWithZeroShield() {
        AEntity entity = mock(AEntity.class);
        ShieldComponent shield = new ShieldComponent(0);

        shield.apply(entity);

        verify(entity, times(1)).addShield(0);
    }

    @Test
    void testApplyWithNegativeShield() {
        AEntity entity = mock(AEntity.class);
        ShieldComponent shield = new ShieldComponent(-10);

        shield.apply(entity);

        verify(entity, times(1)).addShield(-10);
    }

    @Test
    void testMultipleApplications() {
        AEntity entity = mock(AEntity.class);
        ShieldComponent shield = new ShieldComponent(5);

        shield.apply(entity);
        shield.apply(entity);

        verify(entity, times(2)).addShield(5);
    }
}
