package inf112.game.components;

import inf112.game.entity.AEntity;

public class ShieldComponent implements Component {
    private final int shieldAmount;

    public ShieldComponent(int shieldAmount) {
        this.shieldAmount = shieldAmount;
    }

    @Override
    public void apply(AEntity entity) {
        entity.addShield(shieldAmount);
    }
}
