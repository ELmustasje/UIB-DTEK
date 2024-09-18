package no.uib.inf101.sample.rooms;
import no.uib.inf101.sample.equipment.BenchPress;
import no.uib.inf101.sample.equipment.IEquipment;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

public class PlotTest {

    @Test
    void testAddEquipment(){
        Plot p1 = new Plot(0,0,0,0);
        IEquipment bench = new BenchPress(null);
        assertNull(p1.equipment);
        p1.addEquiptment(bench);
        assertEquals(bench,p1.equipment);
    }

    @Test
    void getValues(){
        Plot p1 = new Plot(1,2,3,4);
        assertEquals(1,p1.getX());
        assertEquals(2,p1.getY());
        assertEquals(3,p1.getHeight());
        assertEquals(4,p1.getWidth());
    }
}
