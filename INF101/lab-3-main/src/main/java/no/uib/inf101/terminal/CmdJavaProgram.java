package no.uib.inf101.terminal;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;

public class CmdJavaProgram implements Command {

    private final Class<?> mainClass;
    private final String commandName;

    /**
     * Create a command from a Java program. Programs that read from
     * standard input are not supported, but printing to standard
     * output (i.e. using System.out to print) is ok.
     *
     * @param commandName  The name of the command
     * @param mainClass  The class containing a main(String[]) method
     */
    public CmdJavaProgram(String commandName, Class<?> mainClass) {
        this.mainClass = mainClass;
        this.commandName = commandName;
    }

    @Override
    public String getName() {
        return this.commandName;
    }

    @Override
    public String getManual() {
        return "Sssssssss";
    }

    @Override
    public String run(String[] args) {
        // Remember original System.out and System.err
        final PrintStream orgOut = System.out;
        final PrintStream orgErr = System.err;

        try {
            // Prepare for capturing the output of the program
            ByteArrayOutputStream out = new ByteArrayOutputStream();
            PrintStream newOut = new PrintStream(out, true, StandardCharsets.UTF_8);
            System.setOut(newOut);
            System.setErr(newOut);

            // Invoke  main method of the class for which this command was created
            Method main = this.mainClass.getMethod("main", String[].class);
            main.invoke(null, new Object[] {args});

            // Convert the captured output to a string and return it
            return out.toString(StandardCharsets.UTF_8);
        } catch (NoSuchMethodException e) {
            return "No main method found in class " + this.mainClass.getName();
        } catch (Exception e) {
            return e.toString();
        } finally {
            // Restore the original System.out and System.err
            System.setOut(orgOut);
            System.setErr(orgErr);
        }
    }
}
