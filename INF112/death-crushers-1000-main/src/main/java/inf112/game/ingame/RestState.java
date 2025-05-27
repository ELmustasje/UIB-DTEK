package inf112.game.ingame;

import inf112.core.state.AState;
import inf112.core.state.StateManager;
import inf112.gfx.Sprite;
import inf112.gfx.ui.Background;
import inf112.gfx.ui.Button;
import inf112.gfx.ui.Label;
import inf112.utils.Vec2;

public class RestState extends AState {

    private final GameManager game;
    private StateManager manager;
    private final Background bg;

    public RestState(GameManager game) {
        super();
        this.game = game;

        addComponent(new Label("You take a break and heal 25% of max health...", Vec2.relative(0.42f, 0.4f)));
        addComponent(new Button(b -> manager.switchTo(new MapState(game)), b -> {
        }, Vec2.relative(0.5f, 0.5f), "Continue"));
        bg = new Background(Sprite.BG_REST);
    }

    @Override
    public void init(StateManager manager) {
        this.manager = manager;
        game.player().heal((int) (game.player().maxHealth() * 0.25f));
    }

    @Override
    public void update() {
        stage(bg);
    }

    @Override
    public void clean() {
    }

}
