package inf112.game.splash;

import inf112.core.Config;
import inf112.core.events.WaitGroup;
import inf112.core.state.AState;
import inf112.core.state.StateManager;
import inf112.game.menu.MenuState;
import inf112.gfx.Atlas;
import inf112.gfx.ui.Label;
import inf112.utils.AudioManager;
import inf112.utils.Vec2;

public class SplashState extends AState {

    private WaitGroup group;
    private StateManager manager;
    private AudioManager audio;

    @Override
    public void init(StateManager manager) {
        this.manager = manager;

        group = new WaitGroup();
        audio = AudioManager.getInstance();

        Atlas.loadSprites("atlas.png");

        addComponent(new Label(Config.GAME_NAME, new Vec2(Config.VIEW_WIDTH / 2.f, Config.VIEW_HEIGHT / 2.f - 50)));
        addComponent(new Label(Config.GROUP_NAME, new Vec2(Config.VIEW_WIDTH / 2.f, Config.VIEW_HEIGHT / 2.f)));
        addComponent(new Label(Config.LIBRARY, new Vec2(Config.VIEW_WIDTH / 2.f, Config.VIEW_HEIGHT / 2.f + 50)));

        loadSfx("bap", "gameOver", "hurt", "hit", "gong", "dealCard", "shield", "coins", "gun", "energy");
        loadMusic("battle", "combat0", "combat1", "combat2", "combat3", "mainmenu", "boss");
    }

    private void loadSfx(String... names) {
        for (String name : names) {
            audio.loadSfx(name, "soundeffect/" + name + ".wav");
            group.add(() -> audio.sfxLoaded(name));
        }
    }

    private void loadMusic(String... names) {
        for (String name : names) {
            AudioManager.getInstance().loadMusic(name, "soundtrack/" + name + ".mp3");
            group.add(() -> audio.musicLoaded(name));
        }
    }

    @Override
    public void update() {
        group.update();

        if (group.done()) {
            manager.awaitSwitch(new MenuState(), 1f);
        }
    }

    @Override
    public void clean() {
    }
}
