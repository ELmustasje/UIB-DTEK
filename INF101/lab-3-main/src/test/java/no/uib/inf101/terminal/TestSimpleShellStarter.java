package no.uib.inf101.terminal;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.File;
import java.io.IOException;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class TestSimpleShellStarter {
    static final String orgCwd = System.getProperty("user.dir");
    static final String DIR = "testdir";
    static final String SUBDIR = "subdir";
    private File dir;
    private File subdir;
    private SimpleShell shell;

    ////////////////////////////////////////////////////////////////////////
    //////// The tests  ////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////

    @Test
    public void testDoNothing() {
        assertEquals("$ ", shell.getScreenContent());
    }

    @Test
    public void testWriteFoo() {
        shell.keyPressed('f');
        shell.keyPressed('o');
        shell.keyPressed('o');

        assertEquals("$ foo", shell.getScreenContent());
    }

    @Test
    public void testIllegalCommand() {
        shell.keyPressed('f');
        shell.keyPressed('o');
        shell.keyPressed('o');
        shell.keyPressed('\n');

        assertEquals("$ foo\nCommand not found: \"foo\"\n$ ", shell.getScreenContent());
    }

    @Test
    public void testPwd() throws IOException {
        shell.keyPressed('p');
        shell.keyPressed('w');
        shell.keyPressed('d');
        shell.keyPressed('\n');

        String expected = "$ pwd" + "\n" + this.dir.getCanonicalPath() + "\n$ ";
        assertEquals(expected, shell.getScreenContent());
    }

    @Test
    public void testLs() throws IOException {
        shell.keyPressed('l');
        shell.keyPressed('s');
        shell.keyPressed('\n');

        String expected = "$ ls\n" + SUBDIR + " \n$ ";
        assertEquals(expected, shell.getScreenContent());
    }

    @Test
    public void testCd() throws IOException {
        shell.keyPressed('p');
        shell.keyPressed('w');
        shell.keyPressed('d');
        shell.keyPressed('\n');

        shell.keyPressed('c');
        shell.keyPressed('d');
        shell.keyPressed(' ');
        shell.keyPressed('s');
        shell.keyPressed('u');
        shell.keyPressed('b');
        shell.keyPressed('d');
        shell.keyPressed('i');
        shell.keyPressed('r');
        shell.keyPressed('\n');

        shell.keyPressed('p');
        shell.keyPressed('w');
        shell.keyPressed('d');
        shell.keyPressed('\n');


        String expected = "$ pwd\n"
            + this.dir.getCanonicalPath() + "\n"
            + "$ cd subdir\n"
            + "$ pwd\n"
            + this.subdir.getCanonicalPath() + "\n"
            + "$ ";

        assertEquals(expected, shell.getScreenContent());
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
        this.shell = new SimpleShell();
    }

    @AfterEach
    public void tearDown() {
        File cwd = new File(System.getProperty("user.dir"));
        File testDir = new File(cwd, DIR);
        deleteFolderAndItsContent(testDir);
        System.setProperty("user.dir", orgCwd);
        this.shell = null;
    }
    private void deleteFolderAndItsContent(File file) {
        if (file.isDirectory()) {
            for (File f : file.listFiles()) {
                deleteFolderAndItsContent(f);
            }
        }
        file.delete();
    }

}
