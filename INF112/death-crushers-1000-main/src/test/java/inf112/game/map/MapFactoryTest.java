package inf112.game.map;

import inf112.game.map.nodes.MapEdge;
import inf112.game.map.nodes.MapNode;
import inf112.game.map.nodes.NodeType;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

public class MapFactoryTest {

    @Test
    public void testGenerateMapStructure() {
        long seed = 12345L;
        GameMap map = MapFactory.generateMap(seed);
        int rows = map.getNumRows();
        int cols = map.getNumCols();

        MapNode start = map.getStartNode();
        assertNotNull(start, "Start node should not be null");
        assertEquals(NodeType.START, start.getNodeType(), "Start node must be of type START");
        assertEquals(rows - 1, start.getRow(), "Start node should be on the bottom row");
        assertEquals(cols / 2, start.getCol(), "Start node should be horizontally centered");

        MapNode boss = map.getNode(0, cols / 2);
        assertNotNull(boss, "Boss node should not be null");
        assertEquals(NodeType.BOSS, boss.getNodeType(), "Boss node must be of type BOSS");

        List<MapNode> battleRow = map.getRow(rows - 2);
        for (MapNode n : battleRow) {
            if (n != null) {
                assertEquals(NodeType.BATTLE, n.getNodeType(), "Battle row nodes must be BATTLE");
            }
        }

        List<MapNode> restRow = map.getRow(1);
        for (MapNode n : restRow) {
            if (n != null) {
                assertEquals(NodeType.REST, n.getNodeType(), "Rest row nodes must be REST");
            }
        }
    }

    @Test
    public void testEdgesConnectAdjacentFloors() {
        long seed = 7777L;
        GameMap map = MapFactory.generateMap(seed);
        for (MapEdge e : map.getEdges()) {
            int diff = Math.abs(e.src().getRow() - e.dst().getRow());
            assertEquals(1, diff, "Edge should connect nodes on adjacent floors (diff = 1), but got diff = " + diff);
        }
    }

    @Test
    public void testReproducibility() {
        long seed = 99999L;
        GameMap map1 = MapFactory.generateMap(seed);
        GameMap map2 = MapFactory.generateMap(seed);

        assertEquals(map1.getNumRows(), map2.getNumRows(), "Both maps should have the same number of rows");
        assertEquals(map1.getNumCols(), map2.getNumCols(), "Both maps should have the same number of columns");

        for (int r = 0; r < map1.getNumRows(); r++) {
            List<MapNode> row1 = map1.getRow(r);
            List<MapNode> row2 = map2.getRow(r);
            for (int c = 0; c < map1.getNumCols(); c++) {
                MapNode n1 = row1.get(c);
                MapNode n2 = row2.get(c);
                if (n1 == null || n2 == null) {
                    assertEquals(n1, n2, "Both maps should have the same node at (" + r + "," + c + ")");
                } else {
                    assertEquals(n1.getNodeType(), n2.getNodeType(),
                            "Nodes at (" + r + "," + c + ") should be of the same type");
                }
            }
        }

        assertEquals(map1.getEdges().size(), map2.getEdges().size(), "Both maps should have the same number of edges");
    }
}
