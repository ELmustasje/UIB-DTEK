package inf112.game.ingame;

import inf112.core.state.AState;
import inf112.core.state.StateManager;

public class TreasureState extends AState {

    private final GameManager game;

    public TreasureState(GameManager game) {
        super();
        this.game = game;
    }

    @Override
    public void init(StateManager manager) {
        manager.switchTo(new ChooseCardState(game));
    }

    @Override
    public void update() {

    }

    @Override
    public void clean() {
    }

}
