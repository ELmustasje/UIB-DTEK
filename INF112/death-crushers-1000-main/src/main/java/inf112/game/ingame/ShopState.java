package inf112.game.ingame;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import inf112.core.Config;
import inf112.core.events.EventQueue;
import inf112.core.state.AState;
import inf112.core.state.StateManager;
import inf112.game.deck.ACard;
import inf112.game.deck.Deck;
import inf112.gfx.Anchor;
import inf112.gfx.Container;
import inf112.gfx.ui.Button;
import inf112.gfx.ui.CoinAmount;
import inf112.gfx.ui.Label;
import inf112.utils.Vec2;

public class ShopState extends AState {

    private final GameManager game;
    private List<ACard> shopCards;
    Map<ACard, Vec2> originalPositions = new HashMap<>();
    Map<ACard, Label> priceLabels = new HashMap<>();
    private CoinAmount coins;
    private EventQueue queue;

    public ShopState(GameManager game) {
        super();
        this.game = game;
    }

    @Override
    public void init(StateManager manager) {
        queue = new EventQueue();
        coins = new CoinAmount(new Vec2(100, 50), game.player().getGold());

        final int centerX = Config.VIEW_WIDTH / 2;
        addComponent(new Label("Programmerbar", new Vec2(centerX, 10)));

        // Continue button
        addComponent(new Button(manager, new MapState(game), new Vec2(Config.VIEW_WIDTH / 2.f, Config.VIEW_HEIGHT - 150.f),
                "Continue"));

        shopCards = Deck.getNRandomCards(5);

        final int cardW = 270;
        final int cardH = 420;
        final int gap = 10;
        int totalWidth = shopCards.size() * (cardW + gap) - gap;
        int startX = (Config.VIEW_WIDTH - totalWidth) / 2;

        for (int i = 0; i < shopCards.size(); i++) {
            ACard card = shopCards.get(i);
            int cardx = startX + (cardW + gap) * i;
            int cardy = Config.VIEW_HEIGHT / 2 - cardH / 2;
            Vec2 basePos = new Vec2(cardx, cardy);
            card.pos = new Vec2(cardx, cardy);
            originalPositions.put(card, basePos);

            Label priceLabel = new Label("$" + card.price(), new Vec2(cardx + cardW / 2.f, cardy + cardH + 10.f), 1,
                    Anchor.CENTER);
            priceLabels.put(card, priceLabel);
        }
    }

    @Override
    public void update() {
        for (Label l : priceLabels.values()) {
            stage(l);
        }

        stage((Container) coins);
        coins.set(game.player().getGold());

        for (ACard card : shopCards) {
            Vec2 basePos = originalPositions.get(card);
            Vec2 targetPos = new Vec2(basePos.x, basePos.y);

            if (queue.done()) {
                if (card.hovered()) {
                    targetPos = new Vec2(basePos.x, basePos.y - 30);
                    if (card.clicked()) {
                        if (game.player().purchase(card.price())){

                            priceLabels.remove(card);
                            game.deck().addToDeck(card);
                            
                            queue.add(() -> card.pos.accelerateTowards(new Vec2(Config.VIEW_WIDTH, Config.VIEW_HEIGHT), 2));
                            queue.add(() -> {
                                shopCards.remove(card);
                                return true;
                            });
                        }
                    }
                }

                card.pos.accelerateTowards(targetPos, 3f);
            }

            stage((Container) card);
        }

        queue.update();
    }

    @Override
    public void clean() {
    }

}
