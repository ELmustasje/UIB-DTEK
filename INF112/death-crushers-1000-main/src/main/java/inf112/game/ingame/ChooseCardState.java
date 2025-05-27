package inf112.game.ingame;

import java.util.List;

import inf112.core.Config;
import inf112.core.events.EventQueue;
import inf112.core.state.AState;
import inf112.core.state.StateManager;
import inf112.game.deck.ACard;
import inf112.game.deck.Deck;
import inf112.gfx.Container;
import inf112.gfx.ui.Button;
import inf112.gfx.ui.Label;
import inf112.utils.Vec2;

public class ChooseCardState extends AState {

    private StateManager manager;
    private final GameManager game;
    private List<ACard> chooseCards;
    private EventQueue queue;
    private boolean selected;

    final int cardW = 270;
    final int cardH = 420;

    public ChooseCardState(GameManager game) {
        super();
        this.game = game;
    }

    @Override
    public void init(StateManager manager) {
        this.manager = manager;
        this.queue = new EventQueue();
        this.selected = false;

        chooseCards = Deck.getNRandomCards(3);
        for (ACard c : chooseCards)
            c.pos.set(Config.VIEW_WIDTH / 2.f - cardW / 2.f, Config.VIEW_HEIGHT / 2.f - cardH / 2.f);

        addComponent(new Label("Choose a card to obtain", new Vec2(Config.VIEW_WIDTH / 2.f - 125, 200)));

        addComponent(
                new Button(manager, new MapState(game), new Vec2(Config.VIEW_WIDTH / 2.f - 25, Config.VIEW_HEIGHT - 200.f),
                        "Skip"));
    }

    @Override
    public void update() {
        final int gap = 10;
        int totalWidth = chooseCards.size() * (cardW + gap) - gap;
        int startX = (Config.VIEW_WIDTH - totalWidth) / 2;

        if (!selected) {
            for (ACard card : chooseCards) {
                Vec2 target = new Vec2(startX, Config.VIEW_HEIGHT / 2.f - cardH / 2.f);

                if (card.hovered()) {
                    target.y -= 30;
                    if (card.clicked()) {
                        selected = true;

                        game.deck().addToDeck(card);
                        queue.add(() -> card.pos.accelerateTowards(new Vec2(Config.VIEW_WIDTH, Config.VIEW_HEIGHT), 1));
                        queue.add(() -> {
                            manager.switchTo(new MapState(game));
                            return true;
                        });

                        break;
                    }
                }

                card.pos.accelerateTowards(target, 4);
                startX += cardW + gap;
            }
        }

        for (ACard c : chooseCards)
            stage((Container) c);

        queue.update();
    }

    @Override
    public void clean() {
    }

}
