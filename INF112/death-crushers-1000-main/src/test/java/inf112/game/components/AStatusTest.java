package inf112.game.components;

import inf112.game.entity.AEntity;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class AStatusTest {

    class DummyStatus extends AStatus {

        public DummyStatus(Integer rounds) {
            super(rounds);
        }

        @Override
        public void apply(AEntity entity) {
        }
    }

    private DummyStatus status;

    @BeforeEach
    public void setUp() {
        status = new DummyStatus(3);
    }

    @Test
    public void testInitialRounds() {
        assertEquals(3, status.rounds(), "Status should start with 3 rounds");
    }

    @Test
    public void testDecreaseRounds() {
        status.decreaseRounds();
        assertEquals(2, status.rounds(), "Rounds should decrease to 2");

        status.decreaseRounds();
        assertEquals(1, status.rounds(), "Rounds should decrease to 1");

        status.decreaseRounds();
        assertEquals(0, status.rounds(), "Rounds should decrease to 0");
    }

    @Test
    public void testDecreaseRoundsThrowsExceptionWhenExpired() {
        status.decreaseRounds();
        status.decreaseRounds();
        status.decreaseRounds();

        IllegalStateException exception = assertThrows(IllegalStateException.class, () -> {
            status.decreaseRounds();
        });
        assertEquals("Status has already expired", exception.getMessage());
    }
}
