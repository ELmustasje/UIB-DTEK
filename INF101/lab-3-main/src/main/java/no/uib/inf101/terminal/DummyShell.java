package no.uib.inf101.terminal;

// UiB INF101 ShellLab - DynamicShell.java
//
// Denne koden er gitt som et eksempel på en klasse som implementerer
// CommandLineInterface. Du trenger ikke gjøre noen endringer i denne
// filen for denne lab'en (med mindre du selv ønsker).

public class DummyShell implements CommandLineInterface {

  private String screenContent = "$ ";

  @Override
  public void keyPressed(char key) {
    if (key == '\n') {
      screenContent += "\n$ ";
    } else {
      screenContent += key;
    }
  }

  @Override
  public String getScreenContent() {
    return screenContent;
  }
}
