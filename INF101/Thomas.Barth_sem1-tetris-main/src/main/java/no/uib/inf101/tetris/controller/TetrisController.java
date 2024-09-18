package no.uib.inf101.tetris.controller;

import no.uib.inf101.tetris.midi.TetrisSong;
import no.uib.inf101.tetris.model.GameState;
import no.uib.inf101.tetris.view.TetrisView;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

public class TetrisController implements java.awt.event.KeyListener{
    ControllableTetrisModel model;
    TetrisView view;
    Timer timer;
    TetrisSong song = new TetrisSong();

    /**
     * Constructs a TetrisController object with the specified ControllableTetrisModel and TetrisView.
     * Initializes the timer and starts the TetrisSong.
     *
     * @param model the ControllableTetrisModel to control
     * @param view the TetrisView to update
     */
    public TetrisController(ControllableTetrisModel model, TetrisView view){
        this.model = model;
        this.view = view;
        view.setFocusable(true);
        view.addKeyListener(this);
        this.timer = new Timer(model.getMillis(), this::clockTick);
        timer.start();
        song.run();
    }

    /**
     * Updates the timer delay and initial delay based on the model's millisecond value.
     */
    private void updateTimer(){
        timer.setDelay(model.getMillis());
        timer.setInitialDelay(model.getMillis());
    }

    /**
     * Handles the clock tick event.
     * If the game state is GAME_OVER, returns immediately.
     * Otherwise, updates the model, timer, and view, and repaints the view.
     *
     * @param event the ActionEvent representing the clock tick event
     */
    void clockTick(ActionEvent event){
        if(model.getGameState() == GameState.GAME_OVER){
            return;
        }
        model.clockTick();
        updateTimer();
        view.repaint();
    }




    /**
     * Handles the key pressed event.
     * If the game state is GAME_OVER, returns immediately.
     * Otherwise, performs the corresponding action based on the pressed key:
     * - VK_LEFT: moves the tetromino to the left
     * - VK_RIGHT: moves the tetromino to the right
     * - VK_DOWN: moves the tetromino down and restarts the timer
     * - VK_UP: rotates the tetromino
     * - VK_SPACE: drops the tetromino and restarts the timer
     * Finally, repaints the view.
     *
     * @param e the KeyEvent representing the key pressed event
     */
    @Override
    public void keyPressed(KeyEvent e) {
        if(model.getGameState() == GameState.GAME_OVER){
            return;
        }
        if (e.getKeyCode() == KeyEvent.VK_LEFT) {
            model.moveTetromino(0,-1);
        }
        else if (e.getKeyCode() == KeyEvent.VK_RIGHT) {
            model.moveTetromino(0,1);
        }
        else if (e.getKeyCode() == KeyEvent.VK_DOWN) {
            model.moveTetromino(1,0);
            timer.restart();
        }
        else if (e.getKeyCode() == KeyEvent.VK_UP) {
            model.rotateTetromino();
        }
        else if (e.getKeyCode() == KeyEvent.VK_SPACE) {
            model.dropTetromino();
            timer.restart();
        }
        view.repaint();
    }


    // Other methods from the KeyListener interface are not used and left empty
    @Override
    public void keyReleased(KeyEvent e) {}
    @Override
    public void keyTyped(KeyEvent e) {}
}
