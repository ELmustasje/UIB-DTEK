package inf112.game.components;

public abstract class AStatus implements Component {

    private Integer rounds;

    /**
     * Create a new status with a given number of rounds
     *
     * @param rounds The number of rounds the status should last
     */
    public AStatus(Integer rounds) {
        this.rounds = rounds;
    }

    /**
     * Returns the number of rounds left for the status
     */
    public Integer rounds() {
        return rounds;
    }

    /**
     * Decrease the number of rounds left for the status
     */
    public void decreaseRounds() {
        if (rounds <= 0) {
            throw new IllegalStateException("Status has already expired");
        }
        rounds--;
    }
}
