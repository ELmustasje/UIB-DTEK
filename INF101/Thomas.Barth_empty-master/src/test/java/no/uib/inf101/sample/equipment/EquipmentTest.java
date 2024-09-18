package no.uib.inf101.sample.equipment;
import static org.junit.jupiter.api.Assertions.*;

import no.uib.inf101.sample.rooms.Plot;
import no.uib.inf101.sample.sprites.Sprite;
import org.junit.jupiter.api.Test;

import java.awt.*;

public class EquipmentTest {

    @Test
    void testAddToPlot(){
        Plot p1 = new Plot(100,100,50,50);
        IEquipment bench = new BenchPress(null);
        bench.addToPlot(p1);
        assertEquals(new Point(p1.getX(),p1.getY()),new Point(bench.getX(),bench.getY()));
        assertEquals(new Point(p1.getWidth(),p1.getHeight()), new Point(bench.getWidth(),bench.getHeight()));
    }

    @Test
    void TestChangeDifficulty(){
        Sprite sprite = new Sprite(null);
        IEquipment bench = new BenchPress(null);
        sprite.updateStrength(10);
        bench.changeDifficulty(sprite);
        assertEquals(10,sprite.getStrength());
        assertEquals(2,bench.getSkipInterval());
    }

    @Test
    void TestUseAndIsInUse(){
        IEquipment bench = new BenchPress(null);
        assertFalse(bench.isInUse());
        bench.use();
        assertTrue(bench.isInUse());
    }

    @Test
    void TestSuccessfulSet(){
        IEquipment bench = new BenchPress(null);
        assertFalse(bench.successfulSet());
    }

    @Test
    void TestGetValues(){
        IEquipment bench = new BenchPress(null);
        Plot p1 = new Plot(1,2,200,300);
        bench.addToPlot(p1);
        assertEquals(1,bench.getX());
        assertEquals(2,bench.getY());
        assertEquals(200,bench.getHeight());
        assertEquals(300,bench.getWidth());
    }
}
