package no.uib.inf101.sample.rooms;

import no.uib.inf101.sample.GamePanel;
import no.uib.inf101.sample.MouseHandler;
import no.uib.inf101.sample.equipment.flappyBird.ArcadeMachine;
import no.uib.inf101.sample.equipment.IEquipment;
import no.uib.inf101.sample.sprites.ISprite;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

public class RoomThree implements IRoom{


    private BufferedImage img;
    private final Plot plot;

    /**
     * Constructor for RoomThree. It initializes the room with default values and sets up the plot with equipment.
     *
     * @param mouseHandler The MouseHandler instance for managing mouse events.
     */
    public RoomThree(MouseHandler mouseHandler){
        setDefaultValues();
        plot = new Plot(500,20,400,300);
        plot.addEquiptment(new ArcadeMachine(mouseHandler));
    }

    @Override
    public void drawRoom(Graphics2D g2) {
        g2.drawImage(img,0,0, GamePanel.WIDTH,GamePanel.HEIGHT,null);
        plot.draw(g2);
    }


    @Override
    public void setDefaultValues() {
        try {
            img = ImageIO.read(new File("res/rooms/roomThree.png"));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void setSpriteToRoom(ISprite sprite) {
        sprite.mirrorTurn(false);
        sprite.setValues(250,170,sprite.getWidth(),sprite.getHeight());
    }

    @Override
    public void update() {
        plot.update();
    }

    @Override
    public Point getNextRoomButton() {
        return null;
    }
    @Override
    public Point getPrevRoomButton() {
        return new Point(50,200);
    }

    @Override
    public IEquipment getPlot() {
        return plot.equipment;
    }
}
