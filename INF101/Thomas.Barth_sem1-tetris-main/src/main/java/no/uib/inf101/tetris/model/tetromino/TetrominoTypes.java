package no.uib.inf101.tetris.model.tetromino;

import java.util.HashMap;
import java.util.Map;

public abstract class TetrominoTypes {
    /**
     * Retrieves the shape of a Tetromino type identified by its symbol.
     *
     * @param symbol The character symbol representing the Tetromino type.
     * @return The shape of the Tetromino identified by the given symbol.
     * @throws IllegalArgumentException If the symbol is not found in the shape map.
     */
    protected static boolean[][] getShape(Character symbol){
        if (!shapeMap.containsKey(symbol)) {
            throw new IllegalArgumentException();
        }
        return shapeMap.get(symbol);
    }

    /**
     * Mapping of Tetromino symbols to their respective shapes.
     */
    static Map<Character,boolean[][]> shapeMap = new HashMap<>(){{
        put('L',new boolean[][]{
                {false,false,false},
                {true,true,true},
                {true,false,false}
        });
        put('J',new boolean[][]{
                {false,false,false},
                {true,true,true},
                {false,false,true}
        });
        put('S',new boolean[][]{
                {false,false,false},
                {false,true,true},
                {true,true,false}
        });
        put('Z',new boolean[][]{
                {false,false,false},
                {true,true,false},
                {false,true,true}
        });
        put('T',new boolean[][]{
                {false,false,false},
                {true,true,true},
                {false,true,false}
        });
        put('I',new boolean[][]{
                {false,false,false,false},
                {true,true,true,true},
                {false,false,false,false},
                {false,false,false,false}
        });
        put('O',new boolean[][]{
                {false,false,false,false},
                {false,true,true,false},
                {false,true,true,false},
                {false,false,false,false}
        });
    }};

}
