package labyrinth.model.board;

import java.util.List;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Random;
import java.util.Scanner;

import grid.CellPosition;
import grid.Grid;
import grid.GridCell;
import grid.GridDirection;
import labyrinth.model.board.items.LabyrinthTile;

public class LabyrinthBoard extends Grid<Character> {

    private static final String FILE_PATH = "src/main/java/labyrinth/model/board";

    public static LabyrinthBoard BOARD1 = readBoardFromFile(FILE_PATH + "/defaultBoards/Board1.txt");
    public static LabyrinthBoard BOARD2 = readBoardFromFile(FILE_PATH + "/defaultBoards/Board2.txt");
    public static LabyrinthBoard TEST_BOARD1 = readBoardFromFile(FILE_PATH + "/defaultBoards/TestBoard1.txt");
    public static LabyrinthBoard TEST_BOARD2 = readBoardFromFile(FILE_PATH + "/defaultBoards/TestBoard2.txt");
    public static LabyrinthBoard TEST_BOARD3 = readBoardFromFile(FILE_PATH + "/defaultBoards/TestBoard3.txt");
    public static LabyrinthBoard TEST_BOARD4 = readBoardFromFile(FILE_PATH + "/defaultBoards/TestBoard4.txt");

    public LabyrinthBoard(int rows, int cols) {
        super(rows, cols);
        for (int row = 0; row < rows; row++) {
            for (int col = 0; col < cols; col++) {
                CellPosition pos = new CellPosition(row, col);
                this.set(pos, LabyrinthTile.OPEN.getSymbol());
            }
        }
    }

    /**
     * Create a new board with randomly placed walls
     * 
     * @param rows
     * @param cols
     * @return random board
     */
    public static LabyrinthBoard makeRandomBoard(int rows, int cols) {
        Random random = new Random();
        LabyrinthBoard board = new LabyrinthBoard(rows, cols);
        for (int row = 0; row < rows; row++) {
            for (int col = 0; col < cols; col++) {
                int r = random.nextInt(20);
                LabyrinthTile tile;
                if (r < 15) {
                    tile = LabyrinthTile.OPEN;
                } else {
                    tile = LabyrinthTile.WALL;
                }
                board.set(new CellPosition(row, col), tile.getSymbol());
            }
        }
        return board;
    }

    public static LabyrinthBoard readBoardFromFile(String filename) {
        LabyrinthBoard board = null;
        try {
            File file = new File(filename);
            Scanner sc = new Scanner(file);
            String[] dimensions = sc.nextLine().split(" ");
            int rows = Integer.parseInt(dimensions[0]);
            int cols = Integer.parseInt(dimensions[1]);
            board = new LabyrinthBoard(rows, cols);

            int row = 0;
            while (sc.hasNextLine()) {
                String line = sc.nextLine();
                for (int i = 0; i < line.length(); i++) {
                    CellPosition pos = new CellPosition(row, i);
                    char c = line.charAt(i);
                    board.set(pos, c);
                }
                row++;
            }
            sc.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        return board;
    }

    public List<CellPosition> openTiles() {
        return getTilesOfType(LabyrinthTile.OPEN);
    }

    public List<CellPosition> wallTiles() {
        return getTilesOfType(LabyrinthTile.WALL);
    }

    private List<CellPosition> getTilesOfType(LabyrinthTile tile) {
        List<CellPosition> posList = new ArrayList<>();
        for (GridCell<Character> cell : this) {
            CellPosition pos = cell.pos();
            char c = cell.value();
            if (c == tile.getSymbol())
                posList.add(pos);
        }
        return posList;
    }

    /**
     * Get neighbouring open tiles. If the neighbour is a wall it is ignored and not
     * added to the list of cell positions.
     * 
     * @param pos
     * @return list of neighbouring tiles
     */
    public List<CellPosition> neighbours(CellPosition pos) {
        List<CellPosition> neighbours = new ArrayList<>();
        for (GridDirection dir : GridDirection.values()) {
            try {
                CellPosition neigh = pos.shiftedBy(dir);
                if (this.get(neigh) != LabyrinthTile.WALL.getSymbol()) {
                    neighbours.add(neigh);
                }
            } catch (IndexOutOfBoundsException e) {
                // Do nothing
            }
        }
        return neighbours;
    }

}
