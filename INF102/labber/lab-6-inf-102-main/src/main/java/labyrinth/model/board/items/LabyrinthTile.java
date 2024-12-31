package labyrinth.model.board.items;

public enum LabyrinthTile {
    WALL('*'),
    OPEN(' '),
    PLAYER('s'),
    MONSTER('m'),
    GOLD('g');

    private final char symbol;

    /**
     * @param symbol Unique symbol of the tile
     */
    LabyrinthTile(char symbol) {
        this.symbol = symbol;
    }

    /**
     * @return The symbol to represent this tile
     */
    public char getSymbol() {
        return symbol;
    }

}
