package no.uib.inf101.terminal;

// UiB INF101 ShellLab - Context.java
//
// Denne filen inneholder kode som navigerer et filsystem. Du trenger
// ikke gjøre noen endringer i denne filen for denne lab'en (med mindre
// du selv ønsker).

import java.io.File;
import java.io.IOException;
import java.util.Objects;

public class Context {

  ////////////////////////
  // Instance variables
  ////////////////////////
  private final File home;
  private File cwd;

  ////////////////////////
  // Constructors
  ////////////////////////

  /**
   * Create a new context with the java current working directory as
   * home.
   */
  public Context() {
    this(new File(System.getProperty("user.dir")));
  }

  /**
   * Create a new context with the given directory as home.
   *
   * @param home  the home directory.
   */
  public Context(File home) {
    try {
      this.home = home.getCanonicalFile();
      this.cwd = this.home;
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  ////////////////////////
  // Instance methods
  ////////////////////////

  /**
   * Go to the given path, if it exists. The path can be relative to
   * the current working directory, or absolute.
   *
   * @param path  the path to go to.
   * @return  true if the path exists, false otherwise
   */
  public boolean goToPath(String path) {
    File newDir = new File(this.cwd, path);
    if (!newDir.isDirectory()) {
      return false;
    }
    try {
      this.cwd = newDir.getCanonicalFile();
      return true;
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  /**
   * Get the current working directory.
   *
   * @return  the current working directory.
   */
  public File getCwd() {
    return this.cwd;
  }

  /** Set the current working directory to the home directory.*/
  public void goToHome() {
    this.cwd = this.home;
  }

  /**
   * Check if the current working directory is the home directory.
   *
   * @return  true if the current working directory is the home
   *          directory, false otherwise.
   */
  public boolean isAtHome() {
    return Objects.equals(this.cwd, this.home);
  }
}
