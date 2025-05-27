package inf112.game.components;

import inf112.game.entity.AEntity;

public class Throttled implements Component {

    @Override
    public void apply(AEntity entity) {
        entity.applyDamageModifier(-0.25f);
    }

}
