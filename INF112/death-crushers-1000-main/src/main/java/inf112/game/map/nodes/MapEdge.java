package inf112.game.map.nodes;

/**
 * Represents a directed edge (connection) between two {@link MapNode}s.
 * Used in {@link GameMap} to track the structure of the map graph.
 *
 * <p>
 * Edges are considered directional from {@code src} to {@code dst}.
 * </p>
 */
public record MapEdge(MapNode src, MapNode dst) {
}
