package no.uib.inf101.terminal;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class TestCmdEcho {
    public void testGetName() {
    Command echo = new CmdEcho();
    assertEquals("echo", echo.getName());
  }

  @Test
  public void testRun() {
    Command echo = new CmdEcho();

    String[] args = new String[] {"foo", "bar"};
    assertEquals("foo bar ", echo.run(args));

    args = new String[] {"hello"};
    assertEquals("hello ", echo.run(args));

    args = new String[] {};
    assertEquals("", echo.run(args));
  }
}
