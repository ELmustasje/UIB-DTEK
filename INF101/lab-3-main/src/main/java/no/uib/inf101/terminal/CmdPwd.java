package no.uib.inf101.terminal;

public class CmdPwd implements Command{
    Context context = new Context();

    @Override
    public String run(String[] args) {
        return this.context.getCwd().getAbsolutePath();
    }

    @Override
    public String getName() {
        return "pwd";
    }

    @Override
    public String getManual() {
        return "t";
    }

    @Override
    public void setContext(Context context) {
        this.context = context;
    }
}
