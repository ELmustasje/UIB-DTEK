package no.uib.inf101.sample.equipment.flappyBird;

import no.uib.inf101.sample.MouseHandler;
import no.uib.inf101.sample.equipment.IEquipment;
import no.uib.inf101.sample.rooms.Plot;
import no.uib.inf101.sample.sprites.ISprite;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

/**
 * The ArcadeMachine class implements the IEquipment interface and represents an arcade machine in the game.
 * It encapsulates the functionality of an in-game arcade machine, such as playing a mini-game like Flappy Bird.
 */
public class ArcadeMachine implements IEquipment {

    private int x, y, width, height;

    private boolean overTen, using;
    private final FlappyBird flappyBird;
    private BufferedImage img;

    /**
     * Constructs an ArcadeMachine object with a reference to the MouseHandler.
     * It initializes the FlappyBird mini-game and sets the arcade machine to its default state.
     *
     * @param mouseHandler The MouseHandler instance for managing mouse events related to the ArcadeMachine.
     */
    public ArcadeMachine(MouseHandler mouseHandler){
        flappyBird = new FlappyBird(mouseHandler);
        resetEquipment();
    }


    @Override
    public void use() {
        flappyBird.reset();
        using = true;
    }

    @Override
    public boolean isInUse() {
        return using;
    }

    @Override
    public boolean successfulSet() {
        return overTen;
    }

    @Override
    public void resetEquipment() {
        flappyBird.reset();
        using = false;
        overTen = false;
    }

    @Override
    public void hovering() {
        try {
            img = ImageIO.read(new File("res/equipment/arcadeMachineGlow.png"));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void setDefaultImg() {
        try {
            img = ImageIO.read(new File("res/equipment/arcadeMachine.png"));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public int getHeight() {
        return height;
    }

    @Override
    public int getSkipInterval() {
        return 0;
    }

    @Override
    public int getWidth() {
        return width;
    }

    @Override
    public int getY() {
        return y;
    }

    @Override
    public int getX() {
        return x;
    }

    @Override
    public void addToPlot(Plot plot) {
        this.x = plot.getX();
        this.y = plot.getY();
        this.width = plot.getWidth();
        this.height = plot.getHeight();
    }

    @Override
    public void draw(Graphics2D g2) {
        if(!isInUse()){
            g2.drawImage(img,x,y,width,height,null);
        }else {
            flappyBird.draw(g2);
        }

    }


    @Override
    public void drawStats(Graphics2D g2) {

    }

    @Override
    public void update() {
        if(isInUse()){
            if(!flappyBird.isRunning()){
               if(flappyBird.getScore() >= 10){
                   overTen = true;
               }
            }
            flappyBird.update();
            if(flappyBird.done()){
                using = false;
            }
        }
    }

    @Override
    public void changeDifficulty(ISprite sprite) {

    }
}
