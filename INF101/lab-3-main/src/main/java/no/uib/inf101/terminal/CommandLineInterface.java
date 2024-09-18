package no.uib.inf101.terminal;

// UiB INF101 ShellLab - CommandLineInterface.java
//
// Grensesnittet CommanLineInterface beskriver metoder for input og
// output til et program som virker med et tekstbasert grensesnitt.
// Du trenger ikke gjøre noen endringer i denne filen for denne lab'en.

/**
 * A command line interface is a program with a text-based user interface.
 * The user can enter keys as input, and the program will respond by giving
 * text which can be displayed on a screen.
 */
public interface CommandLineInterface {

  /**
   * Called when a key is pressed.
   *
   * @param key  the key that was pressed
   */
  void keyPressed(char key);

  /**
   * Get the text to display on the screen.
   *
   * @return  the text to display
   */
  String getScreenContent();
}
