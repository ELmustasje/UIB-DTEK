package no.uib.inf101.sample.equipment;

import no.uib.inf101.sample.MouseHandler;
import no.uib.inf101.sample.rooms.Plot;
import no.uib.inf101.sample.sprites.ISprite;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

/**
 * The BenchPress class implements the IEquipment interface and represents a bench press equipment in the game.
 * It manages the equipment's state, including the number of repetitions, sets, and interaction with the mouse.
 */
public class BenchPress implements IEquipment{

    private int x;
    private int y;
    private int width;
    private int height;
    private final int buttonX;
    private final int buttonY;
    private final int buttonWidth;
    private final int buttonHeight;

    private int reps,clicksForRep,repsForSet,clickCount,clickReverse,skipFrames,skipInterval;

    private final int exitButtonSize;
    private final int exitButtonX;
    private final int exitButtonY;

    private boolean using, finished;

    private BufferedImage img,buttonImg, exitButton, exitButtonGlow;

    private final MouseHandler mouseHandler;

    /**
     * Constructs a BenchPress object with a reference to the MouseHandler.
     * It initializes the button position, size, and skip interval for the equipment,
     * and sets the equipment to its default state.
     *
     * @param mouseHandler The MouseHandler instance for managing mouse events related to the BenchPress.
     */
    public BenchPress(MouseHandler mouseHandler){
        this.mouseHandler = mouseHandler;
        buttonX = 80;
        buttonY = 270;
        buttonHeight = 80;
        buttonWidth = 80;
        exitButtonX = 100;
        exitButtonY = 400;
        exitButtonSize = 40;
        skipInterval = 23;
        resetEquipment();
    }

    @Override
    public void use() {
        using = true;
    }

    @Override
    public boolean isInUse() {
        return using;
    }

    @Override
    public boolean successfulSet() {
        return finished;
    }

    @Override
    public void resetEquipment() {
        clickReverse = 1;
        clicksForRep = 10;
        repsForSet = 5;
        skipFrames = 0;
        finished = false;
        using = false;
        setDefaultImg();
    }

    @Override
    public void hovering() {
        try {
            img = ImageIO.read(new File("res/equipment/benchpressGlow.png"));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void setDefaultImg() {
        try {
            img = ImageIO.read(new File("res/equipment/benchpress.png"));
            buttonImg  = ImageIO.read(new File("res/Other/button.png"));
            exitButton = ImageIO.read(new File("res/Other/quitbutton.png"));
            exitButtonGlow = ImageIO.read(new File("res/Other/quitbuttonglow.png"));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private boolean hoveringButton() {
        mouseHandler.update();
        return mouseHandler.mouseX >= buttonX && mouseHandler.mouseX <= buttonX + buttonWidth
                && mouseHandler.mouseY >= buttonY && mouseHandler.mouseY <= buttonY + buttonHeight;
    }

    @Override
    public int getHeight() {
        return height;
    }

    @Override
    public int getSkipInterval() {
        return skipInterval;
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
        g2.drawImage(img,x,y,width,height,null);
        if(using){
            drawStats(g2);
            drawExitButton(g2);
        }
    }

    @Override
    public void drawStats(Graphics2D g2) {
        int interval = 170 / (repsForSet + 1);
        g2.setColor(Color.black);
        g2.drawRect(100, 70 + interval, 40, 170 - interval);
        g2.fillRect(100, 240 - interval * reps, 40, interval * reps);
        g2.drawImage(buttonImg, buttonX, buttonY, buttonWidth, buttonHeight, null);

    }
    private void drawExitButton(Graphics2D g2){


        if(!hoveringExitButton()) {
            g2.drawImage(exitButton, exitButtonX, exitButtonY, exitButtonSize, exitButtonSize, null);
        }else {
            g2.drawImage(exitButtonGlow, exitButtonX,exitButtonY, exitButtonSize, exitButtonSize,null);
        }
    }

    private boolean hoveringExitButton(){
        return (
                mouseHandler.mouseX > exitButtonX &&
                        mouseHandler.mouseX < exitButtonX + exitButtonSize &&
                        mouseHandler.mouseY > exitButtonY &&
                        mouseHandler.mouseY < exitButtonY+exitButtonSize
        );
    }



    @Override
    public void update() {
        if(using){
            if(hoveringButton() && mouseHandler.mousePressed){
                clickCount++;
                if (clickCount == clicksForRep){
                    reps++;
                    if(reps == repsForSet){
                        using = false;
                        finished = true;
                        reps = 0;
                    }
                    clickCount=0;
                }
            }
            skipFrames++;
            if(skipFrames >= skipInterval){
                clickCount -= clickReverse;
                if(clickCount < 0){
                    clickCount = 0;
                }
                skipFrames = 0;

            }

            try {
                img = ImageIO.read(new File("res/sprites/spriteBench/"+clickCount+".png"));
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
            if(hoveringExitButton() && mouseHandler.mousePressed){
                using = false;
                reps = 0;
            }
        }

    }

    @Override
    public void changeDifficulty(ISprite sprite) {
        skipInterval = 22 - sprite.getStrength()*2;
    }
}
