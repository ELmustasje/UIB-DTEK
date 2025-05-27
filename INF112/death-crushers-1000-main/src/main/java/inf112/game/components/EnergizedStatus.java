package inf112.game.components;

import inf112.game.entity.AEntity;

public class EnergizedStatus extends AStatus {

    public EnergizedStatus() {
        super(2);
    }

    @Override
    public void apply(AEntity entity) {
        entity.applyDamageModifier(0.25f);
    }

}
