package no.uib.inf101.terminal;

import java.util.HashMap;
import java.util.Map;

public interface Command {
    String run(String[] args);
    String getName();
    String getManual();
    default void setCommandContext(Map<String,Command> map){

    }
    default void setContext(Context context){

    }

}
