package no.uib.inf101.colorgrid;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class TestTextQuestions {

  @Test
  public void q1() {
    assertEquals(5, TextQuestions.a1,
        "A class implementing IColorGrid must implement all " +
            "methods from IColorGrid, including methods inherited from " +
            "GridDimension and CellColorCollection"
    );
  }

  @Test
  public void q2() {
    assertEquals(true, TextQuestions.a2,
        "If an interface A extends interface B, then any " +
            "class implementing A must also implement " +
            "B, and objects in such a class will " +
            "have the type of both interfaces."
    );
  }

  @Test
  public void q3() {
    assertEquals(false, TextQuestions.a3,
        "It is possible to create a class which implements " +
            "CellColorCollection directly, without implementing IColorGrid. " +
            "Objects in such a class would have the type GridDimension, " +
            "but not the type IColorGrid."
    );
  }

  @Test
  public void q4() {
    assertEquals(false, TextQuestions.a4,
        "It is possible to create a class which implements " +
            "GridDimension directly, without implementing IColorGrid or " +
            "CellColorCollection. Objects in such a class would have the " +
            "type GridDimension, but not the type CellColorCollection."
    );
  }

  @Test
  public void q5() {
    assertEquals(true, TextQuestions.a5,
        "If an object has the type IColorGrid, it must indeed " +
            "belong to a class that implements the interface IColorGrid. " +
            "If the class does not do so directly, it must do so " +
            "indirectly through inheritance."
    );
  }

  @Test
  public void q6() {
    assertEquals(true, TextQuestions.a6,
        "If a class implements IColorGrid, then it will also " +
            "implement GridDimension, since IColorGrid extends it. Any " +
            "object in such a class will have the types IColorGrid, " +
            "GridDimension, and CellColorCollection."
    );
  }
}
