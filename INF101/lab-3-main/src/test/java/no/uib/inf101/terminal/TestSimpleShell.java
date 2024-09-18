package no.uib.inf101.terminal;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;

import static org.junit.jupiter.api.Assertions.*;

public class TestSimpleShell {

  static final String orgCwd = System.getProperty("user.dir");
  static final String DIR = "testdir";
  static final String SUBDIR = "subdir";
  private File subdir;
  private File dir;
  private SimpleShell shell; // The shell we are testing

  ////////////////////////////////////////////////////////////////////////
  //////// The tests  ////////////////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////

  @Test
  public void testImplementsCLI() {
    assertInstanceOf(CommandLineInterface.class, this.shell,
        "SimpleShell should implement CommandLineInterface");
  }

  @Test
  public void testDoNothing() {
    if ((Object) this.shell instanceof CommandLineInterface cli) {
      assertEquals("$ ", cli.getScreenContent());
    } else {
      fail("SimpleShell does not implement CommandLineInterface");
    }
  }

  @Test
  public void testWriteFoo() {
    if ((Object) this.shell instanceof CommandLineInterface cli) {
      cli.keyPressed('f');
      cli.keyPressed('o');
      cli.keyPressed('o');

      assertEquals("$ foo", cli.getScreenContent());
    } else {
      fail("SimpleShell does not implement CommandLineInterface");
    }
  }

  @Test
  public void testIllegalCommand() {
    if ((Object) this.shell instanceof CommandLineInterface cli) {
      cli.keyPressed('f');
      cli.keyPressed('o');
      cli.keyPressed('o');
      cli.keyPressed('\n');

      assertEquals("$ foo\nCommand not found: \"foo\"\n$ ", cli.getScreenContent());
    } else {
      fail("SimpleShell does not implement CommandLineInterface");
    }
  }

  @Test
  public void testPwd() throws IOException {
    if ((Object) this.shell instanceof CommandLineInterface cli) {
      cli.keyPressed('p');
      cli.keyPressed('w');
      cli.keyPressed('d');
      cli.keyPressed('\n');

      String expected = "$ pwd" + "\n" + this.dir.getCanonicalPath() + "\n$ ";
      assertEquals(expected, cli.getScreenContent());
    } else {
      fail("SimpleShell does not implement CommandLineInterface");
    }
  }


  @Test
  public void testLs() throws IOException {
    if ((Object) this.shell instanceof CommandLineInterface cli) {
      cli.keyPressed('l');
      cli.keyPressed('s');
      cli.keyPressed('\n');

      String expected = "$ ls\n" + SUBDIR + " \n$ ";
      assertEquals(expected, cli.getScreenContent());
    } else {
      fail("SimpleShell does not implement CommandLineInterface");
    }
  }

  @Test
  public void testCd() throws IOException {
    if ((Object) this.shell instanceof CommandLineInterface cli) {
      for (char c : "pwd\n".toCharArray()) cli.keyPressed(c);
      for (char c : ("cd " + SUBDIR + "\n").toCharArray()) cli.keyPressed(c);
      for (char c : "pwd\n".toCharArray()) cli.keyPressed(c);

      String expected = "$ pwd\n"
          + this.dir.getCanonicalPath() + "\n"
          + "$ cd subdir\n"
          + "$ pwd\n"
          + this.subdir.getCanonicalPath() + "\n"
          + "$ ";

      assertEquals(expected, cli.getScreenContent());
    } else {
      fail("SimpleShell does not implement CommandLineInterface");
    }
  }


  ////////////////////////////////////////////////////////////////////////
  //////// Preparing the tests  //////////////////////////////////////////
  ////////////////////////////////////////////////////////////////////////

  @BeforeEach
  public void setUp() throws IOException {
    // Set up test directory
    File cwd = new File(orgCwd);
    this.dir = new File(cwd, DIR);
    this.subdir = new File(this.dir, SUBDIR);
    this.dir.mkdir();
    this.subdir.mkdir();
    System.setProperty("user.dir", this.dir.getCanonicalPath());

    // Set up shell
    this.shell = new SimpleShell();
  }

  @AfterEach
  public void tearDown() {
    // Tear down test directory
    File cwd = new File(System.getProperty("user.dir"));
    File testDir = new File(cwd, DIR);
    deleteFolderAndItsContent(testDir);
    System.setProperty("user.dir", orgCwd);

    // Tear down shell
    this.shell = null;
  }
  private void deleteFolderAndItsContent(File file) {
    File[] subFiles = file.listFiles();
    if (subFiles != null) {
      for (File f : subFiles) {
        deleteFolderAndItsContent(f);
      }
    }
    file.delete();
  }
}
