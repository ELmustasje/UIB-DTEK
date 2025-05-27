package inf112.game.components;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import inf112.game.entity.AEntity;

public class EnergizedStatusTest {
    private EnergizedStatus energizedStatus;
    private AEntity mockEntity;

    @BeforeEach
    void setUp() {
        energizedStatus = new EnergizedStatus();
        mockEntity = Mockito.mock(AEntity.class);
    }

    @Test
    void testApplyChangesDamageModifier() {
        energizedStatus.apply(mockEntity);
        Mockito.verify(mockEntity).applyDamageModifier(0.25f); // Sjekker at applyDamageModifier(0.25f) kalles
    }
}
