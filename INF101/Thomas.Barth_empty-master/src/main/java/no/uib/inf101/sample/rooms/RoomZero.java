package no.uib.inf101.sample.rooms;

import no.uib.inf101.sample.GamePanel;
import no.uib.inf101.sample.MouseHandler;
import no.uib.inf101.sample.equipment.BenchPress;
import no.uib.inf101.sample.equipment.IEquipment;
import no.uib.inf101.sample.sprites.ISprite;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

public class RoomZero implements IRoom{

    private BufferedImage img;
    private final Plot plot;

    /**
     * Constructor for RoomZero. It initializes the room with default values and sets up the plot with equipment.
     *
     * @param mouseHandler The MouseHandler instance for managing mouse events.
     */
    public RoomZero(MouseHandler mouseHandler){
        setDefaultValues();
        plot = new Plot(0,0,0,0);
        plot.addEquiptment(new BenchPress(mouseHandler));
    }

    @Override
    public void drawRoom(Graphics2D g2) {
        g2.drawImage(img,0,0, GamePanel.WIDTH,GamePanel.HEIGHT,null);
    }


    @Override
    public void setDefaultValues() {
        try {
            img = ImageIO.read(new File("res/rooms/gymlocker.png"));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void setSpriteToRoom(ISprite sprite) {
        sprite.mirrorTurn(false);
        sprite.setValues(100,200,sprite.getWidth(),sprite.getHeight());
    }

    @Override
    public void update() {
        plot.update();
    }

    @Override
    public Point getNextRoomButton() {
        return new Point(700,250);
    }
    @Override
    public Point getPrevRoomButton() {
        return null;
    }

    @Override
    public IEquipment getPlot() {
        return plot.equipment;
    }
}
