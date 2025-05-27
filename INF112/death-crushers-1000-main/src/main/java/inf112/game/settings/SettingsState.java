package inf112.game.settings;

import inf112.core.state.AState;
import inf112.core.state.StateManager;
import inf112.game.menu.MenuState;
import inf112.gfx.Palette;
import inf112.gfx.ui.Button;
import inf112.gfx.ui.Label;
import inf112.gfx.ui.Slider;
import inf112.utils.AudioManager;
import inf112.utils.Vec2;

public class SettingsState extends AState {

    StateManager manager;

    @Override
    public void init(StateManager manager) {
        this.manager = manager;
        addComponent(new Label("Settings",new Vec2(100,100),Palette.FOREGROUND));
        addComponent(new Button(manager, new MenuState(), new Vec2(500,100),"Main Menu"));

        addComponent(new Label("Music settings",new Vec2(100,350),Palette.FOREGROUND));
        addComponent(new Slider(b -> AudioManager.getInstance().setMusicVolume((float) b.getValue() / 100), new Vec2(0,100), new Vec2(400, 350), 1, (int) (AudioManager.getInstance().getMusicVolume() * 100)));
        addComponent(new Button(b -> AudioManager.getInstance().toggleMusic(),b -> {}, new Vec2(850,350), "Mute"));

        addComponent(new Label("SFX settings",new Vec2(100,450),Palette.FOREGROUND));
        addComponent(new Slider(b -> AudioManager.getInstance().setSfxVolume((float) b.getValue() / 100), new Vec2(0,100), new Vec2(400, 450), 1,(int) (AudioManager.getInstance().getSfxVolume() * 100)));
        addComponent(new Button(b -> AudioManager.getInstance().toggleSfx(),b -> {}, new Vec2(850,450), "Mute"));

    }

    @Override
    public void update() {

    }

    @Override
    public void clean() {
    }

}
