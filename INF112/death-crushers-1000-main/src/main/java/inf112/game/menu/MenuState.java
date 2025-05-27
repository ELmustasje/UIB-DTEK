package inf112.game.menu;

import inf112.core.Config;
import inf112.core.state.AState;
import inf112.core.state.StateManager;
import inf112.game.settings.SettingsState;
import inf112.gfx.ui.Button;
import inf112.gfx.ui.Label;
import inf112.utils.AudioManager;
import inf112.utils.Vec2;

public class MenuState extends AState {

    @Override
    public void init(StateManager manager) {
        final int spacing = 75;
        int x = 100;
        int y = Config.VIEW_HEIGHT / 2;

        addComponent(new Label("Warden of Høytek", new Vec2(x, y - 200.f)));

        addComponent(new Button(manager, new DifficultyState(), new Vec2(x, y), "New Game"));
        addComponent(new Button(manager, new SettingsState(), new Vec2(x, (float) y + spacing), "Settings"));
        addComponent(new Button(manager, new ExitState(), new Vec2(x, y + spacing * 2.f), "Quit"));

        AudioManager.getInstance().playMusic("mainmenu", true);
    }

    @Override
    public void update() {
    }

    @Override
    public void clean() {
    }

}
