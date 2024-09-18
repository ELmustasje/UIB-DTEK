package no.uib.inf101.terminal;

// UiB INF101 ShellLab - Main.java
// Dette er filen som inneholder main-metoden.

import java.util.ArrayList;
import java.util.Arrays;

public class Main {

  public static void main(String[] args) {
    // Create a new shell
    SimpleShell shell = new SimpleShell();
    shell.installCommand(new CmdEcho());
    shell.installCommand(new CmdExit());
    shell.installCommand(new CmdMan());
    shell.installCommand(new CmdJavaProgram("hello", HelloWorld.class));
    shell.installCommand(new CmdJavaProgram("new", Main.class));
    // Run the shell in the terminal GUI
    Terminal gui = new Terminal(shell);
    gui.run();

  }
}