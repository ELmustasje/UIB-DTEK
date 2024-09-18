package no.uib.inf101.terminal;

public class CmdEcho implements Command{
    @Override
    public String run(String[] args) {
        StringBuilder returnString = new StringBuilder();
        for(String s : args){
            returnString.append(s + " ");
        }
        return returnString.toString();
    }

    @Override
    public String getName() {
        return "echo";
    }

    @Override
    public String getManual() {
        return "teit";
    }
}
