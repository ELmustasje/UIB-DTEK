package inf112.game.ingame;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.graphics.OrthographicCamera;
import com.badlogic.gdx.utils.TimeUtils;

import inf112.core.Controller;
import inf112.core.state.AState;
import inf112.core.state.IState;
import inf112.core.state.StateManager;
import inf112.game.ingame.combat.CombatState;
import inf112.game.map.GameMap;
import inf112.game.map.nodes.MapEdge;
import inf112.game.map.nodes.MapNode;
import inf112.game.map.nodes.NodeType;
import inf112.gfx.Canvas;
import inf112.gfx.Sprite;
import inf112.gfx.ui.Button;
import inf112.gfx.ui.Line;
import inf112.utils.Vec2;

/**
 * Represents the game state where the player navigates a node-based map.
 * <p>
 * This state handles drawing the map nodes and edges, controlling camera
 * scrolling,
 * and managing player interaction with map nodes. The map is obtained from a
 * {@link GameManager} which provides access to the current game session.
 * This state is responsible for:
 * <ul>
 * <li>Calculating and rendering map layout</li>
 * <li>Handling camera position and scrolling</li>
 * <li>Managing node locking/unlocking</li>
 * <li>Transitioning to other game states when a node is selected</li>
 * </ul>
 */
public class MapState extends AState {
    private StateManager manager;
    private final GameManager game;
    private final GameMap map;

    private final static float MARGIN = 50f;
    private final float nodeSize = 80f;
    private final float xSpacing = 200f;
    private final float ySpacing = 120f;

    private final float startX = 100f;
    private float startY;

    private float totalWidth;

	private boolean cameraInitialized = false;

    /**
     * Creates a new MapState with access to game components through the given
     * GameManager.
     * 
     * @param game the {@link GameManager} that provides access to the map, player,
     *             and deck
     */
    public MapState(GameManager game) {
        super();
        this.game = game;
        map = game.map();
    }

    @Override
    public void init(StateManager manager) {
        this.manager = manager;
    }

    @Override
    public void update() {
        computeMapDimensions();
        handleScrolling();
        stageEdges();
        stageMapNodes();
    }

    @Override
    public void clean() {
        Canvas.getInstance().camController().reset();
    }

    /**
     * Handles horizontal camera scrolling with arrow keys and clamps to map bounds.
     */
    private void computeMapDimensions() {
        Canvas canvas = Canvas.getInstance();

        GameMap map = game.map();
        int rows = map.getNumRows();
        int cols = map.getNumCols();

        // The leftmost row is (rows-1). The rightmost row is 0.
        this.totalWidth = (rows - 1) * xSpacing + nodeSize;
		float totalHeight = (cols - 1) * ySpacing + nodeSize;

        // Center the entire map vertically in the canvas.
        this.startY = (canvas.height() - totalHeight) / 2f;
    }

    /**
     * Handles horizontal camera scrolling and clamps to map bounds.
     */
    private void handleScrolling() {
        if (map.getNumRows() <= 9)
            return;

        Canvas canvas = Canvas.getInstance();
        OrthographicCamera cam = canvas.getCamera();
        float halfW = cam.viewportWidth * cam.zoom / 2f;

        float minCameraX = startX + halfW - MARGIN;
        float maxCameraX = startX + totalWidth - halfW + MARGIN;

        if (!cameraInitialized) {
            cameraInitialized = true;
            cam.position.x = minCameraX;
            cam.position.y = canvas.height() / 2f;
            cam.update();
        }

        float deltaTime = Gdx.graphics.getDeltaTime();

        Controller.update(deltaTime, minCameraX, maxCameraX);
    }

    /**
     * Renders all map edges as lines connecting node centers.
     */
    private void stageEdges() {
        GameMap map = game.map();
        for (MapEdge edge : map.getEdges()) {
            MapNode src = edge.src();
            MapNode dst = edge.dst();
            Vec2 srcCenter = computeNodeCenter(src);
            Vec2 dstCenter = computeNodeCenter(dst);

            stage(new Line(srcCenter, dstCenter, Sprite.MAP_DOT));
        }
    }

    /**
     * Renders all visible map nodes at their calculated positions.
     */
    private void stageMapNodes() {
        GameMap map = game.map();
        for (int r = 0; r < map.getNumRows(); r++) {
            for (int c = 0; c < map.getNumCols(); c++) {
                MapNode node = map.getNode(r, c);
                if (node == null)
                    continue;

                Vec2 pos = getNodePosition(node);
                stageNode(node, pos.x, pos.y);
            }
        }
    }

    /**
     * Renders an individual node, optionally as a clickable button.
     *
     * @param node the node to render
     * @param x    the x-position in world space
     * @param y    the y-position in world space
     */
    private void stageNode(MapNode node, float x, float y) {
        // if (node.getNodeType() != NodeType.START) {
        if (!node.isLocked() && node.getNodeType() != NodeType.START) {
            Button btn = new Button(b -> {
                selectNode(node);
                manager.switchTo(nextState(node));
            }, b -> {
            }, new Vec2(x, y), node.sprite());
            btn.setScale(((float) Math.sin(TimeUtils.millis() * 0.003) + 1) / 5f + 1);
            stage(btn);

        } else {
            node.setPos(new Vec2(x, y));
            stage(node);
        }
    }

    /**
     * Returns the screen center position of a given node.
     *
     * @param node the node whose center should be calculated
     * @return the screen position of the node's center
     */
    Vec2 computeNodeCenter(MapNode node) {
        Vec2 topLeft = getNodePosition(node);
        return new Vec2(
                topLeft.x + nodeSize / 2f,
                topLeft.y + nodeSize / 2f);
    }

    /**
     * Locks the row of a selected node and unlocks its connected next nodes.
     *
     * @param node the node that was selected
     */
    void selectNode(MapNode node) {
        lockRow(node.getRow());
        unlockNextNodes(node);
        map.setCurrentNode(node);
    }

    /**
     * Locks all nodes in a given row.
     *
     * @param row the row index to lock
     */
    private void lockRow(int row) {
        GameMap map = game.map();
        for (MapNode n : map.getRow(row)) {
            if (n != null)
                n.setLocked();
        }
    }

    /**
     * Unlocks all nodes connected forward from the specified node.
     *
     * @param node the source node
     */
    private void unlockNextNodes(MapNode node) {
        for (MapNode next : map.getConnectedNodes(node)) {
            next.setUnlocked();
        }
    }

    /**
     * Calculates the screen position for the top-left corner of a given node.
     *
     * @param node the node whose position is needed
     * @return the screen position for rendering the node
     */
    Vec2 getNodePosition(MapNode node) {
        int row = node.getRow();
        int col = node.getCol();
        GameMap map = game.map();

        float x = startX + ((map.getNumRows() - 1 - row) * xSpacing);
        float y = startY + (col * ySpacing);
        return new Vec2(x, y);
    }

    /**
     * Determines the appropriate game state to transition to based on a node type.
     *
     * @param node the selected node
     * @return the next {@link IState} to enter
     */
    IState nextState(MapNode node) {
        NodeType nodeType = node.getNodeType();
        return switch (nodeType) {
            case START -> null;
            case SHOP -> new ShopState(game);
            case REST -> new RestState(game);
            case TREASURE -> new TreasureState(game);
            default -> new CombatState(game, nodeType);
        };
    }
}
