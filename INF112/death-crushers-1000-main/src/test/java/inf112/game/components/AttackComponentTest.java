package inf112.game.components;

import inf112.game.entity.AEntity;
import org.junit.jupiter.api.Test;

import static org.mockito.Mockito.*;

class AttackComponentTest {

    @Test
    void testApplyDealsCorrectDamage() {
        AEntity entity = mock(AEntity.class);
        AttackComponent attack = new AttackComponent(10);

        attack.apply(entity);

        verify(entity, times(1)).dealDamage(10);
    }

    @Test
    void testApplyWithZeroDamage() {
        AEntity entity = mock(AEntity.class);
        AttackComponent attack = new AttackComponent(0);

        attack.apply(entity);

        verify(entity, times(1)).dealDamage(0);
    }

    @Test
    void testApplyWithNegativeDamage() {
        AEntity entity = mock(AEntity.class);
        AttackComponent attack = new AttackComponent(-5);

        attack.apply(entity);

        verify(entity, times(1)).dealDamage(-5);
    }

    @Test
    void testMultipleApplications() {
        AEntity entity = mock(AEntity.class);
        AttackComponent attack = new AttackComponent(7);

        attack.apply(entity);
        attack.apply(entity);

        verify(entity, times(2)).dealDamage(7);
    }
}
