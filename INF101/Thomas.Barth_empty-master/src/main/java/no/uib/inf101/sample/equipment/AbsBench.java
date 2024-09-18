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
 * The AbsBench class implements the IEquipment interface and represents an abdominal bench equipment in the game.
 * It manages the equipment's state, including the number of slides for a set, and interaction with the mouse.
 */
public class AbsBench implements IEquipment{

    private int x;
    private int y;
    private int width;
    private int height;
    private final int buttonX;
    private int buttonY;
    private final int buttonWidth;
    private final int buttonHeight;

    private int pic;
    private int slidesForSet;
    private int slideCount;
    private int skipInterval;

    private final int exitButtonSize;
    private final int exitButtonX;
    private final int exitButtonY;

    private boolean using, finished,awaitToGoDown;

    private BufferedImage img, buttonImg, exitButton, exitButtonGlow;

    private final MouseHandler mouseHandler;

    /**
     * Constructs an AbsBench object with a reference to the MouseHandler.
     * It initializes the button position, size, and skip interval for the equipment,
     * and sets the equipment to its default state.
     *
     * @param mouseHandler The MouseHandler instance for managing mouse events related to the AbsBench.
     */
    public AbsBench(MouseHandler mouseHandler){
        this.mouseHandler = mouseHandler;
        buttonX = 85;
        buttonY = 320;
        buttonHeight = 80;
        buttonWidth = 80;
        exitButtonX = 103;
        exitButtonY = 420;
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
        pic = 10;
        buttonY = 330;
        slidesForSet = 5;
        finished = false;
        using = false;
        setDefaultImg();
    }

    @Override
    public void hovering() {
        try {
            img = ImageIO.read(new File("res/equipment/absBenchGlow.png"));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void setDefaultImg() {
        try {
            img = ImageIO.read(new File("res/equipment/absBench.png"));
            buttonImg  = ImageIO.read(new File("res/Other/button.png"));
            exitButton = ImageIO.read(new File("res/Other/quitbutton.png"));
            exitButtonGlow = ImageIO.read(new File("res/Other/quitbuttonglow.png"));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private boolean hoveringSlider() {
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
        int interval = 150/slidesForSet;
        g2.setColor(Color.black);
        g2.fillRect(100,100,50,290);
        g2.drawRect(160,100,50,150);
        g2.fillRect(160,250-interval*slideCount,50,interval*slideCount+1);

        if(buttonY < 70){
            buttonY = 70;
        } else if (buttonY > 320){
            buttonY = 320;
        }

        g2.drawImage(buttonImg,buttonX,buttonY,buttonWidth,buttonHeight,null);
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
            if(hoveringSlider() && mouseHandler.mouseHeld){
                mouseHandler.update();
                buttonY = mouseHandler.mouseY-buttonHeight/2;
                pic = ((buttonY)/30)-1;
                if(pic >10) pic = 10;
                if(pic < 0) pic = 0;
                if (buttonY <= 70 && !awaitToGoDown){
                    awaitToGoDown = true;
                    slideCount++;
                    if (slideCount == slidesForSet){
                        using = false;
                        finished = true;
                        slideCount = 0;
                    }
                }
                if (buttonY >= 330){
                    awaitToGoDown = false;
                }
            }else {
                buttonY = 320;
                pic = ((buttonY)/30)-1;
            }

            try {
                img = ImageIO.read(new File("res/sprites/spriteAbsBench/"+ (pic) +".png"));
            } catch (IOException e) {
                e.printStackTrace();
            }
            if(hoveringExitButton() && mouseHandler.mousePressed){
                using = false;
                slideCount = 0;
            }
        }
    }

    @Override
    public void changeDifficulty(ISprite sprite) {
        skipInterval = 22 - sprite.getStrength()*2;
    }
}
