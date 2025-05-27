package inf112.game.components;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import inf112.game.entity.AEntity;

class StunStatusTest {

    private StunStatus stunStatus;
    private AEntity mockEntity;

    @BeforeEach
    void setUp() {
        stunStatus = new StunStatus();
        mockEntity = Mockito.mock(AEntity.class);
    }

    @Test
    void testApplyDealsDamage() {
        stunStatus.apply(mockEntity);
        Mockito.verify(mockEntity).dealDamage(7);
    }
}
