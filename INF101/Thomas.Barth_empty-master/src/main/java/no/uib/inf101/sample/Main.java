package no.uib.inf101.sample;

import javax.swing.*;

/**
 * Main class containing the entry point for the application.
 */
public class Main {

  /**
   * The main application window.
   */
  public static JFrame window;

  /**
   * The main method which serves as the entry point for the application.
   * It initializes the main application window and starts the game.
   *
   * @param args Command line arguments passed to the application (not used).
   */
  public static void main(String[] args) {
    // Create a new JFrame object to serve as the main window.
    window = new JFrame();

    // Set the default close operation to exit the application when the window is closed.
    window.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    // Prevent the window from being resizable.
    window.setResizable(false);
    // Set the title of the window.
    window.setTitle("DOUCHEBAG SIMULATOR!!!!");

    // Create a new GamePanel object which will serve as the main content pane of the window.
    GamePanel gamePanel = new GamePanel();
    // Set the content pane of the window to the GamePanel.
    window.setContentPane(gamePanel);

    // Pack the components within the window based on their preferred sizes.
    window.pack();
    // Center the window on the screen.
    window.setLocationRelativeTo(null);
    // Make the window visible.
    window.setVisible(true);

    // Start the game thread which will manage the game loop.
    gamePanel.startGameThread();
  }
}