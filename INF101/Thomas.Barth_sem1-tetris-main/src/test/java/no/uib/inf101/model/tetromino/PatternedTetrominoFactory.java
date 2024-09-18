package no.uib.inf101.model.tetromino;

import no.uib.inf101.tetris.model.tetromino.Tetromino;
import no.uib.inf101.tetris.model.tetromino.TetrominoFactory;


public class PatternedTetrominoFactory implements TetrominoFactory {
    String pattern;
    int currentIndex = 0;

    public PatternedTetrominoFactory(String pattern) {
        this.pattern = pattern;
    }

    @Override
    public Tetromino getNext() {
        Tetromino tet = Tetromino.newTetromino(pattern.charAt(currentIndex));
        if (currentIndex == pattern.length() - 1) {
            currentIndex = 0;
        } else {
            currentIndex++;
        }
        return tet;
    }
}
