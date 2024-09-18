package no.uib.inf101.sample.rooms;

import no.uib.inf101.sample.equipment.IEquipment;

import java.awt.*;

public class Plot {

    private final int x;
    private final int y;
    private final int heigth;
    private final int width;

    /**
     * The equipment currently placed on the plot.
     */
    protected IEquipment equipment;

    /**
     * Constructor for the Plot class. It sets the position and size of the plot.
     *
     * @param x      The x-coordinate of the plot's position.
     * @param y      The y-coordinate of the plot's position.
     * @param heigth The height of the plot.
     * @param width  The width of the plot.
     */
    public Plot(int x,int y,int heigth,int width){
        this.x = x;
        this.y = y;
        this.width = width;
        this.heigth = heigth;
    }

    /**
     * Gets the x-coordinate of the plot's position.
     *
     * @return The x-coordinate of the plot.
     */
    public int getX() {
        return x;
    }

    /**
     * Gets the y-coordinate of the plot's position.
     *
     * @return The y-coordinate of the plot.
     */
    public int getY() {
        return y;
    }

    /**
     * Gets the height of the plot.
     *
     * @return The height of the plot.
     */
    public int getHeight() {
        return heigth;
    }

    /**
     * Gets the width of the plot.
     *
     * @return The width of the plot.
     */
    public int getWidth() {
        return width;
    }

    /**
     * Adds equipment to the plot and associates it with this plot.
     *
     * @param equipment The equipment to be added to the plot.
     */
    protected void addEquiptment(IEquipment equipment){
        this.equipment = equipment;
        equipment.addToPlot(this);
    }

    /**
     * Draws the equipment on the plot using the provided Graphics2D context.
     *
     * @param g2 The Graphics2D object used for drawing operations.
     */
    protected void draw(Graphics2D g2){
        if(equipment == null){
            return;
        }
        equipment.draw(g2);
    }

    /**
     * Updates the state of the equipment on the plot.
     */
    protected void update(){
        equipment.update();
    }

}
