package labyrinth.model;

import grid.CellPosition;
import grid.GridCell;
import grid.GridDimension;
import grid.GridDirection;
import labyrinth.controller.ControllableLabyrinthModel;
import labyrinth.model.board.items.BoardPositionFactory;
import labyrinth.model.board.LabyrinthBoard;
import labyrinth.model.board.items.BoardItem;
import labyrinth.model.board.items.LabyrinthTile;
import labyrinth.model.board.items.Monster;
import labyrinth.model.board.items.Player;
import labyrinth.view.ViewableTetrisModel;

import java.util.*;

public class LabyrinthModel implements ViewableTetrisModel, ControllableLabyrinthModel {

    private LabyrinthBoard board;

    // Board Items
    private BoardItem player;
    private BoardItem monster;

    private int playerScore;
    private GameState gameState;

    public LabyrinthModel(LabyrinthBoard board) {
        this(board, BoardPositionFactory.randomPosition(board), BoardPositionFactory.randomPosition(board));
    }

    public LabyrinthModel(LabyrinthBoard board, CellPosition playerPos, CellPosition monsterPos) {
        this.player = new Player(playerPos);
        this.monster = new Monster(monsterPos);
        this.board = board;
        this.gameState = GameState.ACTIVE_GAME;
    }


    public void moveMonster() {
        CellPosition playerPos = player.getPos();
        CellPosition monsterPos = monster.getPos();
        GridDirection dir = monsterPos.getDirectionTowards(playerPos); // Direction the monster will move


        monster = monster.shiftedBy(BFSDir(monsterPos,playerPos));

        // If monster and player is at same position, game is over
        if (monster.getPos().equals(player.getPos()))
            gameState = GameState.GAME_OVER;
    }

    private GridDirection BFSDir(CellPosition monsterPos, CellPosition playerPos){
        Queue<CellPosition> q = new LinkedList<>();
        Set<CellPosition> visited = new HashSet<>();
        Map<CellPosition, CellPosition> parentMap = new HashMap<>();
        q.add(monsterPos);
        visited.add(monsterPos);

        while (!q.isEmpty()) {
            CellPosition curr = q.poll();
            if (curr.equals(playerPos)) {
                break;
            }
            for(CellPosition n : board.neighbours(curr)){
                if(!visited.contains(n)){
                    parentMap.put(n, curr);
                    visited.add(n);
                    q.add(n);
                }

            }
        }

        List<CellPosition> path = new ArrayList<>();
        CellPosition curr = playerPos;
        while(!curr.equals(monsterPos)){
            path.add(curr);
            curr = parentMap.get(curr);
        }
        Collections.reverse(path);
        return monsterPos.getDirectionTowards(path.get(0));
    }


    @Override
    public void movePlayer(GridDirection dir) {
        if (gameState == GameState.GAME_OVER)
            return;
        if (moveBoardItem(dir, player))
            player = player.shiftedBy(dir);
    }

    private boolean moveBoardItem(GridDirection dir, BoardItem item) {
        BoardItem candidate = item.shiftedBy(dir);
        if (legalMove(candidate)) {
            return true;
        }
        return false;
    }

    private boolean legalMove(BoardItem piece) {
        for (GridCell<Character> cell : piece) {
            if (!board.positionIsOnGrid(cell.pos()))
                return false;
            char charOnBoard = board.get(cell.pos());
            if (charOnBoard == LabyrinthTile.WALL.getSymbol())
                return false;
        }
        return true;
    }

    @Override
    public GridDimension getDimension() {
        return board;
    }

    @Override
    public Iterable<GridCell<Character>> getTilesOnBoard() {
        return board;
    }

    @Override
    public Iterable<GridCell<Character>> getPlayer() {
        return player;
    }

    @Override
    public Iterable<GridCell<Character>> getMonster() {
        return monster;
    }

    @Override
    public int getScore() {
        return playerScore;
    }

    @Override
    public GameState getGameState() {
        return gameState;
    }

    @Override
    public int getTimerDelay() {
        return Math.max(600 - (playerScore * 5), 160);
    }

    @Override
    public void clockTick() {
        if (gameState == GameState.GAME_OVER)
            return;
        moveMonster();
        playerScore++;
    }

    @Override
    public void reset() {
        this.player = new BoardItem(BoardPositionFactory.randomPosition(board), LabyrinthTile.PLAYER);
        this.monster = new BoardItem(BoardPositionFactory.randomPosition(board), LabyrinthTile.MONSTER);

        this.board = LabyrinthBoard.makeRandomBoard(board.rows(), board.cols());
        this.playerScore = 0;

        this.gameState = GameState.ACTIVE_GAME;
    }

}
