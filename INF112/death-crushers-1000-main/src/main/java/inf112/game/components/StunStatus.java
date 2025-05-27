package inf112.game.components;

import inf112.game.entity.AEntity;

public class StunStatus extends AStatus {

    public StunStatus() {
        super(2);
    }

    @Override
    public void apply(AEntity entity) {
        entity.dealDamage(7);
    }

}
