package inf112.game.menu;

import com.badlogic.gdx.Gdx;

import inf112.core.state.AState;
import inf112.core.state.StateManager;

public class ExitState extends AState {

    @Override
    public void init(StateManager manager) {
        Gdx.app.exit();
    }

    @Override
    public void update() {
    }

    @Override
    public void clean() {
    }

}
