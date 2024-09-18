package no.uib.inf101.sample.rooms;

import no.uib.inf101.sample.MouseHandler;
import no.uib.inf101.sample.sprites.ISprite;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

public class RoomManager{

    IRoom room;
    ISprite sprite;
    MouseHandler mouseHandler;
    ArrayList<IRoom> rooms = new ArrayList<>();
    Point nextButton;
    Point prevButton;
    BufferedImage roomControlButton;
    BufferedImage roomControlButtonGlow;
    boolean nextButtonGlow;
    boolean prevButtonGlow;
    int roomIndex;


    /**
     * Constructor for the RoomManager class. It initializes the rooms, loads control button images,
     * and sets the initial room.
     *
     * @param sprite       The sprite representing the player or main character.
     * @param mouseHandler The MouseHandler instance for managing mouse events.
     */
    public RoomManager(ISprite sprite, MouseHandler mouseHandler) {
        this.mouseHandler = mouseHandler;
        rooms.add(new RoomZero(mouseHandler));
        rooms.add(new RoomOne(mouseHandler));
        rooms.add(new RoomTwo(mouseHandler));
        rooms.add(new RoomThree(mouseHandler));
        roomIndex = 0;
        this.room = rooms.get(roomIndex);
        this.sprite = sprite;
        room.setSpriteToRoom(sprite);

        try {
            roomControlButton = ImageIO.read(new File("res/Other/RoomButton.png"));
            roomControlButtonGlow = ImageIO.read(new File("res/Other/RoomButtonGlow.png"));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

    }

    /**
     * Draws the current room and the room control buttons if applicable.
     *
     * @param g2 The Graphics2D object used for drawing operations.
     */
    public void draw(Graphics2D g2) {
        room.drawRoom(g2);
        if(!room.getPlot().isInUse()){
            drawRoomControlButtons(room,g2);
        }
    }


    private void drawRoomControlButtons(IRoom room, Graphics2D g2){
        nextButton = room.getNextRoomButton();
        prevButton = room.getPrevRoomButton();
        if(nextButton!= null) {
            if(nextButtonGlow){
                g2.drawImage(roomControlButtonGlow,nextButton.x,nextButton.y,80,60,null);
            }else {
                g2.drawImage(roomControlButton,nextButton.x,nextButton.y,80,60,null);
            }

        }
        if(prevButton!= null) {
            if(prevButtonGlow){
                g2.drawImage(roomControlButtonGlow,prevButton.x+80,prevButton.y,-80,60,null);
            }else {
                g2.drawImage(roomControlButton,prevButton.x+80,prevButton.y,-80,60,null);
            }

        }
    }

    private boolean hoveringNextButton(){
        return (
                nextButton != null
                &&mouseHandler.mouseX >= nextButton.x
                && mouseHandler.mouseX <= nextButton.x+80
                && mouseHandler.mouseY >= nextButton.y
                && mouseHandler.mouseY <= nextButton.y+60
                );
    }
    private boolean hoveringPrevButton(){
        return (
                prevButton != null
                        &&mouseHandler.mouseX >= prevButton.x
                        && mouseHandler.mouseX <= prevButton.x+80
                        && mouseHandler.mouseY >= prevButton.y
                        && mouseHandler.mouseY <= prevButton.y+60
        );
    }

    private boolean hoveringRoomPlot(){
        int margin = 4;
        return (
                mouseHandler.mouseX >= room.getPlot().getX() + room.getPlot().getWidth()/margin
                && mouseHandler.mouseX <= room.getPlot().getX() + room.getPlot().getWidth() - room.getPlot().getWidth()/margin
                && mouseHandler.mouseY >= room.getPlot().getY() + room.getPlot().getHeight()/margin
                && mouseHandler.mouseY <= room.getPlot().getY() +room.getPlot().getHeight() - room.getPlot().getHeight()/margin
        );
    }

    private void changeRoom(int deltaIndex){
        roomIndex+=deltaIndex;
        if(roomIndex > rooms.size()-1) roomIndex = rooms.size()-1;
        if(roomIndex < 0) roomIndex = 0;
        room = rooms.get(roomIndex);
        room.setSpriteToRoom(sprite);
    }


    /**
     * Updates the state of the current room, including handling mouse interactions with room control buttons
     * and the room plot.
     */
    public void update() {
        mouseHandler.update();
        if(hoveringRoomPlot()&&!room.getPlot().isInUse()){
            room.getPlot().hovering();
            if(mouseHandler.mousePressed){
                room.getPlot().use();
            }
        }else {
            room.getPlot().setDefaultImg();
        }

        if(room.getPlot().isInUse()){
            sprite.stopDrawing();
        }else {
            room.setDefaultValues();
            sprite.startDrawing();

            if (room.getPlot().successfulSet()){
                if(room instanceof RoomThree){
                    sprite.updateStrength(-3);
                }else {
                    sprite.updateStrength(1);
                }
                room.getPlot().resetEquipment();
                room.getPlot().changeDifficulty(sprite);
            }
            if(hoveringNextButton()){
                nextButtonGlow = true;
                if (mouseHandler.mousePressed){
                    changeRoom(1);
                    mouseHandler.used();
                }
            }else nextButtonGlow =false;

            if(hoveringPrevButton()){
                prevButtonGlow = true;
                if(mouseHandler.mousePressed){
                    changeRoom(-1);
                    mouseHandler.used();
                }
            }else prevButtonGlow = false;
        }
        room.update();
    }

}
