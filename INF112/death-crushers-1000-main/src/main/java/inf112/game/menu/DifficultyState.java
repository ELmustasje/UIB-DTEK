package inf112.game.menu;

import inf112.core.Config;
import inf112.core.state.AState;
import inf112.core.state.StateManager;
import inf112.game.ingame.InGameState;
import inf112.gfx.Palette;
import inf112.gfx.ui.Button;
import inf112.gfx.ui.Label;
import inf112.utils.Vec2;

public class DifficultyState extends AState {

    @Override
    public void init(StateManager manager) {
        int y = Config.VIEW_HEIGHT / 2;
        int x = Config.VIEW_WIDTH / 2 - 100;
        int space = 50;
        addComponent(new Label("Select difficulty", new Vec2(x, y - 100.f), Palette.ORANGE_LIGHT));
        addComponent(new Button(b -> manager.switchTo(new InGameState(1)), null, new Vec2(x, y), "Easy"));
        addComponent(new Button(b -> manager.switchTo(new InGameState(2)), null, new Vec2(x, (float) y + space), "Normal"));
        addComponent(new Button(b -> manager.switchTo(new InGameState(3)), null, new Vec2(x, y + space * 2.f), "Hard"));
    }

    @Override
    public void update() {
    }

    @Override
    public void clean() {
    }

}
