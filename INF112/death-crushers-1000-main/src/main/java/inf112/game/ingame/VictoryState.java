package inf112.game.ingame;

import inf112.core.Config;
import inf112.core.state.AState;
import inf112.core.state.StateManager;
import inf112.gfx.Container;
import inf112.gfx.ui.Button;
import inf112.gfx.ui.CoinAmount;
import inf112.gfx.ui.Label;
import inf112.utils.AudioManager;
import inf112.utils.Vec2;

public class VictoryState extends AState {

    private final GameManager game;
    private CoinAmount coins;

    public VictoryState(GameManager game) {
        super();
        this.game = game;
    }

    @Override
    public void init(StateManager manager) {
        AudioManager.getInstance().stopMusic();
        AudioManager.getInstance().playSfx("coins");

        int n = (int) (Math.random() * 30) + 10 * (game.map().floor() + 1) + 5 * game.difficulty();
        game.player().addGold(n);

        addComponent(new Label("Rewards", new Vec2(Config.VIEW_WIDTH / 2.f, Config.VIEW_HEIGHT / 2.f - 200), 1));
        coins = new CoinAmount(new Vec2(Config.VIEW_WIDTH / 2.f - 10, Config.VIEW_HEIGHT / 2.f - 150), n);

        addComponent(
                new Button(manager, new ChooseCardState(game), new Vec2(Config.VIEW_WIDTH / 2.f, Config.VIEW_HEIGHT / 2.f),
                        "Continue"));
    }

    @Override
    public void update() {
        stage((Container) coins);
    }

    @Override
    public void clean() {

    }
}
