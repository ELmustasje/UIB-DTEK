package no.uib.inf101.sample;


import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

/**
 * The MouseHandler class implements the MouseListener interface and is responsible for tracking mouse events.
 * It provides functionality to detect mouse position and button states.
 */
public class MouseHandler implements MouseListener {
    /**
     * The current x-coordinate of the mouse cursor.
     */
    public int mouseX;
    /**
     * The current y-coordinate of the mouse cursor.
     */
    public int mouseY;
    /**
     * Indicates whether the mouse button has been pressed.
     */
    public boolean mousePressed;
    /**
     * Indicates whether the mouse button is currently being held down.
     */
    public boolean mouseHeld;

    /**
     * Constructor for MouseHandler. It initializes the mouse state by updating its position.
     */
    public MouseHandler() {
        update();
    }

    /**
     * Updates the mouse position by querying the current pointer info and adjusting for the window's position.
     */
    public void update() {
        Point mouseLocation = MouseInfo.getPointerInfo().getLocation();
        mouseX = mouseLocation.x - Main.window.getX() - 5;
        mouseY = mouseLocation.y - Main.window.getY() - 30;
    }

    /**
     * Resets the mousePressed state to false.
     * This is done to ensure that it is registered as pressed for one update().
     * Typically called after the mouse event has been processed.
     */
    public void used() {
        mousePressed = false;
    }


    @Override
    public void mouseClicked(MouseEvent e) {

    }

    @Override
    public void mousePressed(MouseEvent e) {
        mousePressed = true;
        mouseHeld = true;
    }

    @Override
    public void mouseReleased(MouseEvent e) {
        mousePressed = false;
        mouseHeld = false;
    }

    @Override
    public void mouseEntered(MouseEvent e) {

    }

    @Override
    public void mouseExited(MouseEvent e) {

    }
}
