package labyrinth.controller;

import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.Timer;

import grid.GridDirection;
import labyrinth.model.GameState;
import labyrinth.view.LabyrinthView;

import java.awt.event.ActionEvent;

public class LabyrinthController implements KeyListener {

    private ControllableLabyrinthModel model;
    private LabyrinthView view;

    private Timer timer;

    public LabyrinthController(ControllableLabyrinthModel model, LabyrinthView view) {
        this.model = model;
        this.view = view;
        this.timer = new Timer(model.getTimerDelay(), this::clockTick);

        view.addKeyListener(this);
        view.setFocusable(true);

        this.timer.start();
    }

    public void clockTick(ActionEvent e) {
        if (model.getGameState() == GameState.GAME_OVER)
            return;

        timer.setDelay(model.getTimerDelay());
        model.clockTick();
        view.repaint();
    }

    @Override
    public void keyPressed(KeyEvent e) {
        if (e.getKeyCode() == KeyEvent.VK_LEFT) {
            model.movePlayer(GridDirection.WEST);
        }
        else if (e.getKeyCode() == KeyEvent.VK_RIGHT) {
            model.movePlayer(GridDirection.EAST);
        }
        else if (e.getKeyCode() == KeyEvent.VK_DOWN) {
            model.movePlayer(GridDirection.SOUTH);
        }
        else if (e.getKeyCode() == KeyEvent.VK_UP) {
            model.movePlayer(GridDirection.NORTH);
        }
        else if (e.getKeyCode() == KeyEvent.VK_R) {
            model.reset();
            timer.restart();
        }
        view.repaint();
    }

    @Override
    public void keyReleased(KeyEvent e) {

    }

    @Override
    public void keyTyped(KeyEvent e) {

    }
}