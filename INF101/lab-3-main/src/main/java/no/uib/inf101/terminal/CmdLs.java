package no.uib.inf101.terminal;

import java.io.File;

public class CmdLs implements Command{

    Context context = new Context();

    @Override
    public String run(String[] args) {
        File cwd = this.context.getCwd();
        String s = "";
        for (File file : cwd.listFiles()) {
            s += file.getName();
            s += " ";
        }
        return s;

    }

    @Override
    public String getName() {
        return "ls";
    }

    @Override
    public String getManual() {
        return "g";
    }

    @Override
    public void setContext(Context context) {
        this.context = context;
    }


}
