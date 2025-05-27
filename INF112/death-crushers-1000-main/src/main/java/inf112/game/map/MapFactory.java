package inf112.game.map;

import inf112.core.Config;
import inf112.game.map.nodes.MapNode;
import inf112.game.map.nodes.NodeType;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;

/**
 * A factory class for generating procedurally generated {@link GameMap}
 * instances
 * with structured rules and randomness. This class controls map layout, node
 * types,
 * edge connectivity, and ensures maps are playable and consistent.
 *
 * <p>
 * Features of generated maps:
 * <ul>
 * <li>Start node is always placed at the bottom center</li>
 * <li>Next row is guaranteed to be BATTLE nodes connected to START</li>
 * <li>Second-to-top row contains only REST nodes</li>
 * <li>Top row contains a single BOSS node</li>
 * <li>No elite nodes appear on floors below 6</li>
 * <li>Randomized room types in middle rows with no orphaned nodes</li>
 * </ul>
 */
public class MapFactory {
    private static final int ROWS = Config.MAP_ROWS;
    private static final int COLUMNS = Config.MAP_COLUMNS;

    /**
     * Random distribution:
     * BATTLE=57%, ELITE=20%, REST=15%, SHOP=6%, TREASURE=2%.
     */
    private static final int BATTLE_CUTOFF = 60; // 0..56 => BATTLE
    private static final int REST_CUTOFF = 70; // 77..91 => REST
    private static final int SHOP_CUTOFF = 90; // 92..97 => SHOP
    // 98..99 => TREASURE

    private static final Random random = new Random();

    /**
     * Generates a map with a specific seed for deterministic map generation.
     * If the seed is {@code null}, a new seed is generated and printed to stdout.
     *
     * @param seed the random seed to use, or null to generate a new one
     * @return a new {@link GameMap} based on the provided seed
     */
    public static GameMap generateMap(Long seed) {
        if (seed != null) {
            random.setSeed(seed);
        } else {
            seed = random.nextLong();
            System.out.println("Using random seed: " + seed);
            random.setSeed(seed);
        }

        GameMap map = new GameMap(ROWS, COLUMNS);
        MapNode startNode = map.getStartNode();

        List<MapNode> rowBelow = createBattleRow(map, ROWS - 2);
        for (MapNode n : rowBelow) {
            map.addEdge(startNode, n);
        }

        for (int row = ROWS - 3; row >= 2; row--) {
            List<MapNode> currentRow = createRowWithRules(map, row);
            connectRows(map, currentRow, rowBelow);
            rowBelow = currentRow;
        }

        List<MapNode> restRow = createRestRow(map);
        connectRows(map, restRow, rowBelow);
        rowBelow = restRow;

        MapNode bossNode = map.setNode(0, COLUMNS / 2, NodeType.BOSS);

        connectRows(map, List.of(bossNode), rowBelow);

        lockAllNodes(map);
        unlockStartingNodes(map);

        System.out.println(map);
        return map;
    }

    /**
     * Creates a row of BATTLE nodes in the specified row.
     *
     * @param map
     * 		the map to place nodes in
     * @param rowIndex
     * 		the row index
     * @return a list of created {@link MapNode}s
     */
    private static List<MapNode> createBattleRow(GameMap map, int rowIndex) {
        int numNodes = 2 + random.nextInt(3 - 2 + 1);
        List<Integer> columns = pickColumns(numNodes);
        List<MapNode> rowNodes = new ArrayList<>();
        for (int col : columns) {
            MapNode node = map.setNode(rowIndex, col, NodeType.BATTLE);
            rowNodes.add(node);
        }
        return rowNodes;
    }

    /**
     * Creates a row of REST nodes.
     *
     * @param map
     * 		the map to place nodes in
     * @return a list of created {@link MapNode}s
     */
    private static List<MapNode> createRestRow(GameMap map) {
        int numNodes = 2 + random.nextInt(4 - 2 + 1);
        List<Integer> columns = pickColumns(numNodes);
        List<MapNode> rowNodes = new ArrayList<>();
        for (int col : columns) {
            MapNode node = map.setNode(1, col, NodeType.REST);
            rowNodes.add(node);
        }
        return rowNodes;
    }

    /**
     * Creates a row of nodes with randomized types according to spawn rules.
     *
     * @param map
     * 		the map to place nodes in
     * @param rowIndex
     * 		the row index
     * @return a list of created {@link MapNode}s
     */
    private static List<MapNode> createRowWithRules(GameMap map, int rowIndex) {
        int numNodes = 2 + random.nextInt(4 - 2 + 1);
        List<Integer> columns = pickColumns(numNodes);


        List<MapNode> rowNodes = new ArrayList<>();
        for (int col : columns) {
            NodeType type = pickRandomType();
            MapNode node = map.setNode(rowIndex, col, type);
            rowNodes.add(node);
        }
        return rowNodes;
    }

    /**
     * Picks the specified number of distinct columns at random and returns them
     * in ascending order.
     */
    private static List<Integer> pickColumns(int numNodes) {
        List<Integer> allCols = new ArrayList<>();
        for (int c = 0; c < COLUMNS; c++) {
            allCols.add(c);
        }
        Collections.shuffle(allCols, random);
        if (numNodes > allCols.size()) {
            numNodes = allCols.size();
        }
        List<Integer> chosen = allCols.subList(0, numNodes);
        chosen.sort(Integer::compareTo);
        return new ArrayList<>(chosen);
    }

    /**
     * Connects every node in the upper row to at least one node in the lower row,
     * ensuring no orphans and no crossing edges.
     *
     * @param map      the game map
     * @param upperRow nodes from the upper row (closer to BOSS)
     * @param belowRow nodes from the lower row (closer to START)
     */
    private static void connectRows(GameMap map, List<MapNode> upperRow, List<MapNode> belowRow) {
        if (upperRow.isEmpty() || belowRow.isEmpty())
            return;

        int i = 0;
        int j = 0;
        while (i < upperRow.size() && j < belowRow.size()) {
            map.addEdge(upperRow.get(i), belowRow.get(j));
            // Move pointer that points to the smaller column
            if (upperRow.get(i).getCol() <= belowRow.get(j).getCol()) {
                i++;
            } else {
                j++;
            }
        }

        while (i < upperRow.size()) {
            map.addEdge(upperRow.get(i), belowRow.get(belowRow.size() - 1));
            i++;
        }

        while (j < belowRow.size()) {
            map.addEdge(upperRow.get(upperRow.size() - 1), belowRow.get(j));
            j++;
        }

        for (int x = 0; x < upperRow.size(); x++) {
            int y = Math.min(x, belowRow.size() - 1);
            if (y + 1 < belowRow.size()) {
                maybeAddEdge(map, upperRow.get(x), belowRow.get(y + 1));
            }
        }
    }

    /**
     * Randomly adds an additional edge between nodes with a 40% chance.
     */
    private static void maybeAddEdge(GameMap map, MapNode src, MapNode dst) {
        if (random.nextInt(100) < 40) {
            map.addEdge(src, dst);
        }
    }

    /**
     * Randomly selects a {@link NodeType} for a node based on predefined
     * probabilities.
     * Skips ELITE if the floor is below 6.
     *
     * @return the selected {@link NodeType}
     */
    private static NodeType pickRandomType() {
        int r = random.nextInt(100);
        if (r < BATTLE_CUTOFF) {
            return NodeType.BATTLE;
        } else if (r < REST_CUTOFF) {
            return NodeType.REST;
        } else if (r < SHOP_CUTOFF) {
            return NodeType.SHOP;
        } else {
            return NodeType.TREASURE;
        }
    }

    /**
     * Locks all nodes on the map except the START node.
     */
    private static void lockAllNodes(GameMap map) {
        int rows = map.getNumRows();
        int cols = map.getNumCols();

        for (int row = 0; row < rows; row++) {
            for (int col = 0; col < cols; col++) {
                MapNode node = map.getNode(row, col);
                if (node != null && node.getNodeType() != NodeType.START)
                    map.getNode(row, col).setLocked();
            }
        }
    }

    /**
     * Unlocks all nodes directly connected to the START node.
     */
    private static void unlockStartingNodes(GameMap map) {
        for (MapNode next : map.getConnectedNodes(map.getStartNode())) {
            next.setUnlocked();
        }
    }
}
