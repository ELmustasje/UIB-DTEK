package inf112.game.components;

import inf112.game.entity.AEntity;

/**
 * A Component is an object applied to an {@link AEntity}, either to add some
 * effect or manipulate the Entity directly. This is a slight variation on the
 * Command design pattern.
 */
public interface Component {

    /**
     * Applies effect to target entity
     *
     * @param entity The entity to which the component should apply.
     */
    void apply(AEntity entity);
}
