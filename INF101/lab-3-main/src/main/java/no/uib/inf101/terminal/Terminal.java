package no.uib.inf101.terminal;

// UiB INF101 ShellLab - Terminal.java
//
// Denne filen inneholder kode som tegner et vindu på skjermen, som
// viser et CommandLineInterface -program. Du er ikke forventet å
// forstå denne koden denne uken, og du trenger ikke gjøre noen
// endringer i denne filen for denne lab'en (med mindre du selv ønsker).

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;

/**
 * A simple terminal GUI for interacting with command line interfaces.
 */
public class Terminal extends JPanel {
  ////////////////////////
  // Instance variables
  ////////////////////////
  /** The command line interface to interact with. */
  private final CommandLineInterface cli;

  ////////////////////////
  // Constructor
  ////////////////////////
  /** Create a new terminal GUI for the given command line interface. */
  public Terminal(CommandLineInterface cli) {
    this.cli = cli;
    this.setPreferredSize(new Dimension(400, 300));
    this.setFocusable(true);
    this.addKeyListener(new KeyAdapter() {
      @Override
      public void keyPressed(KeyEvent e) {
        Terminal.this.cli.keyPressed(e.getKeyChar());
        Terminal.this.repaint();
      }
    });
  }

  ////////////////////////
  // Instance methods
  ////////////////////////
  @Override
  public void paintComponent(Graphics g) {
    super.paintComponent(g);

    g.setColor(Color.BLACK);
    g.setFont(new Font("Monospaced", Font.PLAIN, 12));
    int lineHeight = g.getFontMetrics().getHeight() + 2;

    // Tegn teksten på skjermen, linje for linje. Potensiell forbedring:
    // hvis det er for mange linjer, vis kun de siste av dem.
    String content = this.cli.getScreenContent();
    int y = 20;
    for (String line : content.split("\n")) {
      g.drawString(line, 5, y);
      y += lineHeight;
    }
  }

  /** Run the terminal GUI in a window frame. */
  public void run() {
    JFrame frame = new JFrame("INF101 Terminal");
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.add(this);
    frame.pack();
    frame.setVisible(true);
  }
}
