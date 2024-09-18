package no.uib.inf101.terminal;

// UiB INF101 ShellLab - SimpleShell.java
//
// Dette er klassen vi skal forbedre i denne lab'en. Slik koden er
// allerede før du begynner på laben, støtter den tre kommandoer:
//
// - cd: Bytt til en annen mappe
// - ls: List opp filer i mappen
// - pwd: Vis sti til nåværende mappe
//
// Vi skal endre denne klassen slik at den
// - kan vises av Terminal.java
// - kan støtte ubegrenset mange kommandoer

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;

// TODO 1: Let SimpleShell implement CommandLineInterface
public class SimpleShell implements CommandLineInterface{

  //////////////////////////////////////////////////////////////////////
  /// Instance variables ///////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////

  /** The prompt to show before each command */
  private final String prompt = "$ ";
  /** The context variable contains cwd and home directories etc. */
  private final Context context = new Context();
  /** A list of historic commands and their outputs */
  private final List<String> history = new ArrayList<>();
  /** The command currently being typed */
  private String currentCommand = "";
  // TODO 4: Create instance variable storing Command objects
  private final HashMap<String,Command> allCommands =new HashMap<>();


  //////////////////////////////////////////////////////////////////////
  /// Public instance methods                                     //////
  /// (methods expected to be used by someone outside this class) //////
  //////////////////////////////////////////////////////////////////////

  /** Constructor for SimpleShell */
  public SimpleShell() {
    // TODO 7-8-9: Install core commands SimpleShell supports (pwd, ls, cd)
    this.installCommand(new CmdLs());
    this.installCommand(new CmdPwd());
    this.installCommand(new CmdCd());
  }

  // TODO 2: rename method to fit new interface, annotate with @Override
  // Note: methods with @Override generally do not need javadoc comments,
  // since the javadoc comment is inherited. You should hence remove the
  // javadoc comment here unless there is something special about this
  // implementation. You should remove to-do comments when you are done.
  /**
   * Called when a key is pressed.
   *
   * @param key The key that was pressed
   */
  @Override
  public void keyPressed(char key) {
    if (key == '\n') {
      this.processCurrentCommandLine();
    } else if (key >= ' ' && key <= '~') {
      this.currentCommand += key;
    } else if (key == '\b') {
        if (currentCommand.length() > 0) {
          int len = currentCommand.length();
          currentCommand = currentCommand.substring(0, len - 1);
        }
    } else {
      // Some special key was pressed (e.g. shift, ctrl), we ignore
    }
  }

  // TODO 3: rename method to fit new interface, annotate with @Override
  /**
   * Get the text which the terminal should display
   *
   * @return the text
   */
  @Override
  public String getScreenContent() {
    String s = "";
    for (String line : this.history) {
      s += line;
    }
    s += this.prompt;
    s += this.currentCommand;
    return s;
  }

  // TODO 5: Add method to install a command
  public void installCommand(Command command){
    command.setContext(this.context);
    this.allCommands.put(command.getName(),command);
    command.setCommandContext(this.allCommands);
  }

  //////////////////////////////////////////////////////////////////////
  /// Private methods                                ///////////////////
  /// (helper methods used internally in this class) ///////////////////
  //////////////////////////////////////////////////////////////////////

  /**
   * Process the current command line. This entails splitting it into
   * a command name and arguments; executing the command; and adding
   * the result to the history.
   */
  private void processCurrentCommandLine() {
    String result = "";
    if (this.currentCommand.length() > 0) {
      String[] args = this.currentCommand.split(" ");
      String commandName = args[0];
      String[] commandArgs = new String[args.length - 1];
      System.arraycopy(args, 1, commandArgs, 0, commandArgs.length);
      result = this.executeCommand(commandName, commandArgs);
      if (result.length() > 0 && result.charAt(result.length() - 1) != '\n') {
        result += '\n';
      }
    }
    this.history.add(this.prompt + this.currentCommand + "\n" + result);
    this.currentCommand = "";
  }

  /**
   * Execute a command with the given name and arguments. The command
   * name could be "ls", "cd", "pwd", etc., and the arguments are the
   * arguments to the command. For example for the command "cd foo", the
   * command name is "cd" and the argument comes in the array ["foo"].
   *
   * @param commandName  The name of the command to execute
   * @param args  The arguments to the command
   * @return  The output of the command
   */
  private String executeCommand(String commandName, String[] args) {
    // TODO 6: Call run on Command object for given commandName if present
    Command command = this.allCommands.get(commandName);
    if(command != null){
      return command.run(args);
    }else {
      return "Command not found: \"" + commandName +"\"";
    }
  }

}
