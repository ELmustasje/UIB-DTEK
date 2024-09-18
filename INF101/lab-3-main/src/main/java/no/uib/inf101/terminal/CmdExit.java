package no.uib.inf101.terminal;

public class CmdExit implements Command{
    @Override
    public String run(String[] args) {
        System.exit(0);
        return null;
    }

    @Override
    public String getName() {
        return "exit";
    }

    @Override
    public String getManual() {
        return "r";
    }
}
