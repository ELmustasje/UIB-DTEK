package no.uib.inf101.sample.rooms;

import no.uib.inf101.sample.equipment.AbsBench;
import no.uib.inf101.sample.equipment.BenchPress;
import no.uib.inf101.sample.sprites.Sprite;
import org.junit.jupiter.api.Test;


import java.awt.*;

import static org.junit.jupiter.api.Assertions.*;

public class RoomTest {

    @Test
    void SetSpriteToRoomTest(){
        Sprite sprite = new Sprite(null);
        IRoom room = new RoomOne(null);
        room.setSpriteToRoom(sprite);
        assertEquals(new Point(sprite.getX(),sprite.getY()),new Point(450,170));
        assertTrue(sprite.isMirrored());
    }

    @Test
    void getPlots() {
        IRoom roomOne = new RoomOne(null);
        IRoom roomTwo = new RoomTwo(null);
        IRoom roomZero = new RoomZero(null);
        assertEquals(roomZero.getPlot().getClass(), new BenchPress(null).getClass());
        assertEquals(roomOne.getPlot().getClass(), new BenchPress(null).getClass());
        assertEquals(roomTwo.getPlot().getClass(), new AbsBench(null).getClass());
    }
}
