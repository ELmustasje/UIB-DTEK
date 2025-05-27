package inf112.game.ingame;

import inf112.core.state.AState;
import inf112.core.state.StateManager;
import inf112.game.menu.MenuState;
import inf112.gfx.ui.Label;
import inf112.utils.Vec2;

public class DeathState extends AState {

    StateManager manager;

    @Override
    public void init(StateManager manager) {
        this.manager = manager;
        addComponent(new Label("You died :(", Vec2.relative(0.5f, 0.5f)));
        manager.awaitSwitch(new MenuState(), 3);
    }

    @Override
    public void update() {
    }

    @Override
    public void clean() {

    }
}
