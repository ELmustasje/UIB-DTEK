package inf112.game.ingame;

import java.util.List;

import inf112.core.Config;
import inf112.core.state.AState;
import inf112.core.state.StateManager;
import inf112.game.deck.ACard;
import inf112.game.deck.Deck;
import inf112.game.deck.cards.ShieldCard;
import inf112.game.deck.cards.StrikeCard;
import inf112.game.deck.cards.StunCard;
import inf112.game.entity.Player;
import inf112.game.map.GameMap;
import inf112.game.map.MapFactory;
import inf112.gfx.Drawable;
import inf112.gfx.Interactable;
import inf112.utils.AudioManager;

public class InGameState extends AState implements GameManager {

    StateManager parentManager;
    StateManager state;
    AudioManager audio;

    private Deck deck;
    private Player player;
    private final GameMap map;
    private final int difficulty;

    public InGameState(int difficulty) {
        super();
        Config.MAP_ROWS = difficulty * 9;
        this.difficulty = difficulty;

        if (Config.DEBUG)
            map = MapFactory.generateMap(Config.RANDOM_SEED);
        else
            map = MapFactory.generateMap((long) (Math.random() * Integer.MAX_VALUE));
    }

    @Override
    public void init(StateManager manager) {
        parentManager = manager;

        // player initiation will be modified when loading prev game state is introduced
        player = new Player(80, 0, "Player");

        audio = AudioManager.getInstance();
        audio.stopMusic(); // From main menu

        deck = new Deck();

        if (Config.DEBUG) {
            for (ACard c : Deck.getAllCards()) {
                deck.addToDeck(c);
            }
            player.addGold(9999);
        } else {
            for (int i = 0; i < 5; i++)
                deck.addToDeck(new StrikeCard());
            for (int i = 0; i < 5; i++)
                deck.addToDeck(new ShieldCard());
            deck.addToDeck(new StunCard());
        }

        deck.reShuffle();
        state = new StateManager(new MapState(this));
    }

    @Override
    public void update() {
        state.update();
    }

    @Override
    public void clean() {
        state.clean();
    }

    @Override
    public Deck deck() {
        return deck;
    }

    @Override
    public GameMap map() {
        return map;
    }

    @Override
    public Player player() {
        return player;
    }

    @Override
    public List<Drawable> drawables() {
        return state.drawables();
    }

    @Override
    public List<Interactable> interactables() {
        return state.interactables();
    }

    @Override
    public void clear() {
        state.clear();
    }

    @Override
    public int difficulty() {
        return difficulty;
    }
}
