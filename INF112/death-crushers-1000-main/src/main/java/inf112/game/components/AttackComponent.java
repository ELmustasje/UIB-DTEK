package inf112.game.components;

import inf112.game.entity.AEntity;

public class AttackComponent implements Component {
    private final int damage;

    public AttackComponent(int damage) {
        this.damage = damage;
    }

    @Override
    public void apply(AEntity entity) {
        entity.dealDamage(damage);
    }
}
