package no.uib.inf101.tetris.model.tetromino;

import java.awt.*;
import java.util.List;
import java.util.Random;
import java.util.Set;

/**
 * The RandomTetrominoFactory class is responsible for creating tetrominos randomly.
 * It implements the TetrominoFactory interface.
 */
public class RandomTetrominoFactory implements TetrominoFactory {
    /**
     * Gets the next random tetromino.
     *
     * @return a randomly generated tetromino
     */
    @Override
    public Tetromino getNext() {
        Random r  = new Random();
        List<Character> keys = TetrominoTypes.shapeMap.keySet().stream().toList();
        return Tetromino.newTetromino(keys.get(r.nextInt(keys.size())));
    }
}

