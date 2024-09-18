package no.uib.inf101.terminal;

import java.util.Map;

public class CmdMan implements Command {
    Map<String, Command> map;

    @Override
    public void setCommandContext(Map<String, Command> map) {
        this.map = map;
    }

    @Override
    public String run(String[] args) {
        if (args.length > 0) {
            return map.get(args[0]).getManual();
        }
        return "no manual found";
    }

    @Override
    public String getName() {
        return "man";
    }

    @Override
    public String getManual() {
        return "også teit";
    }
}