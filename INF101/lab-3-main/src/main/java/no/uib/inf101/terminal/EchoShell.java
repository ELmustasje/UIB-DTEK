package no.uib.inf101.terminal;

// UiB INF101 ShellLab - EchoShell.java
//
// Denne koden er gitt som et eksempel på en klasse som implementerer
// CommanLineInterface. Du trenger ikke gjøre noen endringer i denne
// filen for denne lab'en (med mindre du selv ønsker).

import java.util.ArrayList;

public class EchoShell implements CommandLineInterface {
  ArrayList<String> outputLines = new ArrayList<>();
  String currentLine = "";

  @Override
  public void keyPressed(char key) {
    if (key == '\n') {
      outputLines.add("$ " + currentLine);
      outputLines.add("Oh, an echo! listen: " + currentLine);
      currentLine = "";
    } else {
      currentLine += key;
    }
  }

  @Override
  public String getScreenContent() {
    String result = "";
    for (String line : outputLines) {
      result += line + "\n";
    }
    return result + "$ " + currentLine;
  }
}
