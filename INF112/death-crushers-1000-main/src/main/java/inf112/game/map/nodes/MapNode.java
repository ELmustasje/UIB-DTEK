package inf112.game.map.nodes;

import inf112.game.map.GameMap;
import inf112.gfx.Drawable;
import inf112.gfx.DrawableType;
import inf112.gfx.Sprite;
import inf112.utils.LocPair;
import inf112.utils.Vec2;


/**
 * Represents a single node in a {@link GameMap}, corresponding to a room or
 * event.
 * Each node has a type (e.g., START, BATTLE), grid position, and can be locked
 * or unlocked.
 * Nodes may store references to their parents and siblings for extended logic,
 * and are rendered on screen via their {@link Sprite}.
 */
public class MapNode implements Drawable {
    private final NodeType nodeType;
    private final LocPair<Integer, Integer> location;
    private final Sprite sprite;
    private boolean locked;
    private final Vec2 pos;

    /**
     * Constructs a new {@code MapNode} of a given type and location in the map
     * grid.
     *
     * @param nodeType the type of the node (e.g., BATTLE, SHOP)
     * @param row      the row index in the grid
     * @param col      the column index in the grid
     */
    public MapNode(NodeType nodeType, int row, int col) {
        this.nodeType = nodeType;
        this.location = new LocPair<>(row, col);
        this.locked = false;
        this.sprite = selectSprite(nodeType);
        this.pos = new Vec2(0, 0);
    }

    /**
     * @return the node's type
     */
    public NodeType getNodeType() {
        return nodeType;
    }

    /**
     * @return the row index in the grid
     */
    public int getRow() {
        return location.row();
    }

    /**
     * @return the column index in the grid
     */
    public int getCol() {
        return location.col();
    }

    /**
     * Locks this node, preventing player access.
     */
    public void setLocked() {
        locked = true;
    }

    /**
     * @return true if the node is currently locked
     */
    public boolean isLocked() {
        return locked;
    }

    /**
     * Marks the node as unlocked (available for entry).
     */
    public void setUnlocked() {
        locked = false;
    }

    @Override
    public Sprite sprite() {
        return this.sprite;
    }

    private Sprite selectSprite(NodeType nodeType) {
		return switch (nodeType) {
			case START -> Sprite.MAP_START;
			case BATTLE -> Sprite.MAP_COMBAT;
			case SHOP -> Sprite.MAP_SHOP;
			case REST -> Sprite.MAP_REST;
			case BOSS -> Sprite.MAP_BOSS;
			default -> Sprite.MAP_CARDS;
		};
    }

    @Override
    public String toString() {
        return "NodeType: " + getNodeType() + " Location: " + location;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (!(o instanceof MapNode other))
            return false;
		return this.getRow() == other.getRow() && this.getCol() == other.getCol() && this.nodeType == other.nodeType;
    }

    @Override
    public int hashCode() {
        return getClass().hashCode();
    }

    @Override
    public float width() {
        return 64;
    }

    @Override
    public float height() {
        return 64;
    }

    @Override
    public Vec2 pos() {
        return pos;
    }

    public void setPos(Vec2 v) {
        this.pos.set(v);
    }

    @Override
    public DrawableType type() {
        return DrawableType.SPRITE;
    }
}
