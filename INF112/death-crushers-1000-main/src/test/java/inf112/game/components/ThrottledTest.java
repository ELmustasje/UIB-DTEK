package inf112.game.components;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import inf112.game.entity.AEntity;

class ThrottledTest {

    private Throttled throttled;
    private AEntity mockEntity;

    @BeforeEach
    void setUp() {
        throttled = new Throttled();
        mockEntity = Mockito.mock(AEntity.class);
    }

    @Test
    void testApplyReducesDamage() {
        throttled.apply(mockEntity);
        Mockito.verify(mockEntity).applyDamageModifier(-0.25f); // Sjekker at modifikatoren settes riktig
    }

}
