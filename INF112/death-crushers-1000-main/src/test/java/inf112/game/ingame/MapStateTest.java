package inf112.game.ingame;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import inf112.core.state.IState;
import inf112.core.state.StateManager;
import inf112.game.ingame.combat.CombatState;
import inf112.game.map.GameMap;
import inf112.game.map.nodes.MapNode;
import inf112.game.map.nodes.NodeType;
import inf112.gfx.Canvas;
import inf112.utils.Vec2;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;

class MapStateTest {

    private MapState mapState;
    private GameManager gameManager;
    private StateManager stateManager;
    private GameMap gameMap;
    private Canvas canvas;
    private MockedStatic<Canvas> canvasStatic;

    @BeforeEach
    void setUp() {
        gameManager = mock(GameManager.class);
        stateManager = mock(StateManager.class);
        gameMap = mock(GameMap.class);
        canvas = mock(Canvas.class);

        when(gameManager.map()).thenReturn(gameMap);
        when(canvas.height()).thenReturn(800);

        canvasStatic = mockStatic(Canvas.class);
        canvasStatic.when(Canvas::getInstance).thenReturn(canvas);

        mapState = new MapState(gameManager);
        mapState.init(stateManager);
    }

    @AfterEach
    void tearDown() {
        canvasStatic.close();
    }

    @Test
    void testInitialization() {
        assertNotNull(mapState);
        assertEquals(gameMap, gameManager.map());
    }

    @Test
    void testSelectNode_UpdatesMapAndUnlocksConnections() {
        MapNode selectedNode = mock(MapNode.class);
        when(selectedNode.getRow()).thenReturn(1);
        when(gameMap.getConnectedNodes(selectedNode)).thenReturn(new HashSet<>(List.of()));

        mapState.selectNode(selectedNode);

        verify(gameMap).setCurrentNode(selectedNode);
        verify(gameMap).getRow(1); // Locking row
    }

    @Test
    void testSelectNode_UnlocksConnectedNodes() {
        MapNode selectedNode = mock(MapNode.class);
        MapNode connected1 = mock(MapNode.class);
        MapNode connected2 = mock(MapNode.class);

        when(selectedNode.getRow()).thenReturn(0);
        when(gameMap.getConnectedNodes(selectedNode)).thenReturn(new HashSet<>(List.of(connected1, connected2)));

        mapState.selectNode(selectedNode);

        verify(connected1).setUnlocked();
        verify(connected2).setUnlocked();
    }

    @Test
    void testComputeNodeCenterIsCentered() {
        MapNode node = mock(MapNode.class);
        when(node.getRow()).thenReturn(2);
        when(node.getCol()).thenReturn(1);
        when(gameMap.getNumRows()).thenReturn(5);

        Vec2 center = mapState.computeNodeCenter(node);

        assertNotNull(center);
        assertTrue(center.x > 0);
        assertTrue(center.y > 0);
    }

    @Test
    void testGetNodePositionIsCorrect() {
        MapNode node = mock(MapNode.class);
        when(node.getRow()).thenReturn(3);
        when(node.getCol()).thenReturn(0);
        when(gameMap.getNumRows()).thenReturn(5);

        Vec2 pos = mapState.getNodePosition(node);

        assertNotNull(pos);
        assertTrue(pos.x > 0);
        assertTrue(pos.y >= 0);
    }

    @Test
    void testLockRowCallsSetLockedOnEachNode() {
        MapNode n1 = mock(MapNode.class);
        MapNode n2 = mock(MapNode.class);
        when(gameMap.getRow(2)).thenReturn(Arrays.asList(n1, null, n2));

        MapNode selectedNode = mock(MapNode.class);
        when(selectedNode.getRow()).thenReturn(2);

        mapState.selectNode(selectedNode);

        verify(n1).setLocked();
        verify(n2).setLocked();
    }

    @Test
    void testNextState_CombatNode() {
        MapNode combat = mock(MapNode.class);
        when(combat.getNodeType()).thenReturn(NodeType.BATTLE);

        IState state = mapState.nextState(combat);
        assertTrue(state instanceof CombatState);
    }

    @Test
    void testNextState_NonCombatNodes() {
        MapNode shop = mock(MapNode.class);
        when(shop.getNodeType()).thenReturn(NodeType.SHOP);
        assertTrue(mapState.nextState(shop) instanceof ShopState);

        MapNode rest = mock(MapNode.class);
        when(rest.getNodeType()).thenReturn(NodeType.REST);
        // assertTrue(mapState.nextState(rest) instanceof RestState);

        MapNode treasure = mock(MapNode.class);
        when(treasure.getNodeType()).thenReturn(NodeType.TREASURE);
        assertTrue(mapState.nextState(treasure) instanceof TreasureState);

        MapNode start = mock(MapNode.class);
        when(start.getNodeType()).thenReturn(NodeType.START);
        assertNull(mapState.nextState(start));
    }
}
