package no.uib.inf101.terminal;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class TestTextAnswers {

  @Test
  public void testQ1() {
    assertEquals("CommandLineInterface", TextAnswers.q1);
  }

  @Test
  public void testQ2() {
    assertEquals("DummyShell", TextAnswers.q2);
  }

  @Test
  public void testQ3() {
    assertEquals(true, TextAnswers.q3, "Every interface defines a type, so "
        + "since CommandLineInterface is an interface, it is also a type.");
  }

  @Test
  public void testQ4() {
    assertEquals(true, TextAnswers.q4, "Every class also defines a "
        + "type, so since DummyShell is a class, it must also be a type.");
  }
}
