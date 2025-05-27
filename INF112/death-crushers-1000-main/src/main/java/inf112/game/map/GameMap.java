package inf112.game.map;

import inf112.game.map.nodes.MapEdge;
import inf112.game.map.nodes.MapNode;
import inf112.game.map.nodes.NodeType;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Represents a grid-based game map of nodes with directional edges.
 * <p>
 * Each {@link MapNode} may represent different room types (combat, shop, rest,
 * etc.),
 * and the map structure supports graph-like traversal logic using
 * {@link MapEdge}s.
 */
public class GameMap {
    private MapNode startNode;
    private final List<List<MapNode>> map;
    private final int rows;
    private final int columns;
    private final HashSet<MapEdge> edges;
    private MapNode currentNode;
    private int floor = 0;

    /**
     * Constructs a new empty GameMap with specified dimensions.
     * Should only be called from {@link MapFactory}.
     *
     * @param rows number of rows in the map
     * @param cols number of columns in the map
     */
    protected GameMap(int rows, int cols) {
        this.rows = rows;
        this.columns = cols;
        this.map = new ArrayList<>();
        this.edges = new HashSet<>();
        this.startNode = null;
        initializeMap();
    }

    /**
     * @return the current floor number (used to determine map generation rules)
     */
    public int floor() {
        return floor;
    }

    /**
     * Advances to the next floor, incrementing the internal counter.
     */
    public void nextFloor() {
        floor++;
    }

    /**
     * Places or replaces a node of the given type at a specified grid location.
     *
     * @param row   the row index
     * @param col   the column index
     * @param nType the type of node to place
     * @return the newly created {@link MapNode}
     */
    public MapNode setNode(int row, int col, NodeType nType) {
        if (nType == null)
            throw new IllegalArgumentException("Node type must be provided.");
        if (!validLocation(row, col))
            throw new IllegalArgumentException("Invalid location.");

        MapNode node = new MapNode(nType, row, col);
        this.map.get(row).set(col, node);
        return node;
    }

    /**
     * Retrieves the node at a given map coordinate.
     *
     * @param row row index
     * @param col column index
     * @return the {@link MapNode} at that location, or null
     */
    public MapNode getNode(int row, int col) {
        if (!validLocation(row, col)) {
            throw new IllegalArgumentException("Invalid location.");
        }
        return map.get(row).get(col);
    }

    /**
     * Adds an existing {@link MapEdge} to this map's set of edges.
     *
     * @param edge the edge to add
     * @throws IllegalArgumentException if the edge is null
     */
    public void addEdge(MapEdge edge) {
        if (edge == null)
            throw new IllegalArgumentException("Edge cannot be null.");
        this.edges.add(edge);
    }

    /**
     * Convenience method to create a new {@link MapEdge} between two nodes and add
     * it to this map.
     *
     * @param node  the source node
     * @param other the destination node
     * @throws IllegalArgumentException if either node is null
     */
    public void addEdge(MapNode node, MapNode other) {
        if (node == null || other == null)
            throw new IllegalArgumentException("Nodes cannot be null.");
        addEdge(new MapEdge(node, other));
    }

    /**
     * @return a {@link HashSet} of all edges in this map
     */
    public HashSet<MapEdge> getEdges() {
        return this.edges;
    }

    /**
     * @return the start {@link MapNode} of this map
     * @throws NullPointerException if the start node has not been initialized
     */
    public MapNode getStartNode() {
        if (this.startNode == null)
            throw new NullPointerException("Start node has not yet been initialized.");
        return this.startNode;
    }

    /**
     * @return the number of rows in this map
     */
    public int getNumRows() {
        return rows;
    }

    /**
     * @return the number of columns in this map
     */
    public int getNumCols() {
        return columns;
    }

    /**
     * Returns the list of nodes in a given row.
     *
     * @param row the row index
     * @return a {@link List} of {@link MapNode} objects (some may be null)
     */
    public List<MapNode> getRow(int row) {
        return map.get(row);
    }

    /**
     * Sets the current node to given node
     * 
     * @param currentNode {@link MapNode} to be set as current node
     */
    public void setCurrentNode(MapNode currentNode) {
        if (currentNode != null)
            this.currentNode = currentNode;
    }

    /**
     * Gathers all nodes that are connected from the given node via forward edges.
     * Only edges going to a lower row (i.e. "ahead") are considered.
     *
     * @param node the source {@link MapNode}
     * @return a set of connected nodes
     */
    public Set<MapNode> getConnectedNodes(MapNode node) {
        Set<MapNode> connected = new HashSet<>();
        for (MapEdge e : getEdges()) {
            if (e.src().equals(node)) {
                if (e.dst().getRow() < node.getRow()) {
                    connected.add(e.dst());
                }
            } else if (e.dst().equals(node)) {
                if (e.src().getRow() < node.getRow()) {
                    connected.add(e.src());
                }
            }
        }
        return connected;
    }

    /**
     * Checks if (row, col) is a valid position in this map.
     *
     * @param row the row index
     * @param col the column index
     * @return true if valid, false otherwise
     */
    private boolean validLocation(int row, int col) {
        return (row >= 0 && row < this.rows) && (col >= 0 && col < this.columns);
    }

    /**
     * Initializes the 2D list of nodes to be all null, then creates a default
     * start node at (rows - 1, columns / 2).
     */
    private void initializeMap() {
        for (int i = 0; i < this.rows; i++) {
            List<MapNode> c = new ArrayList<>();
            for (int j = 0; j < this.columns; j++) {
                c.add(j, null);
            }
            this.map.add(c);
        }
        // By default, place a START node in the bottom-middle
        startNode = setNode(rows - 1, columns / 2, NodeType.START);
    }

    /**
     * Returns a string representation of the map, showing each row of nodes
     * (or "NU" for null).
     *
     * @return a string describing the map
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < rows; i++) {
            sb.append("[");
            for (int j = 0; j < columns; j++) {
                MapNode node = map.get(i).get(j);
                if (node == null) {
                    sb.append("NU");
                } else {
                    sb.append(node.getNodeType().toString(), 0, 2);
                }
                if (j < columns - 1) {
                    sb.append(" -- ");
                }
            }
            sb.append("]\n");
        }
        return sb.toString();
    }
}
