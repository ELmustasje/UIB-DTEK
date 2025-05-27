package inf112.game.menu;

import inf112.core.state.AState;
import inf112.core.state.StateManager;
import inf112.gfx.ui.Button;
import inf112.gfx.ui.Label;
import inf112.utils.Vec2;

public class EndState extends AState {

    @Override
    public void init(StateManager manager) {
        addComponent(new Label("You win!", Vec2.relative(0.5f, 0.4f)));
        addComponent(new Button(manager, new MenuState(), Vec2.relative(0.5f, 0.5f), "Main menu"));
    }

    @Override
    public void update() {
    }

    @Override
    public void clean() {
    }

}
