package inf112.game.ingame.combat;

import inf112.core.Config;
import inf112.core.events.EventQueue;
import inf112.core.events.WaitGroup;
import inf112.core.state.AState;
import inf112.core.state.StateManager;
import inf112.game.components.AStatus;
import inf112.game.deck.ACard;
import inf112.game.deck.Deck;
import inf112.game.deck.cards.ProcrastinationCard;
import inf112.game.entity.*;
import inf112.game.ingame.ChooseCardState;
import inf112.game.ingame.DeathState;
import inf112.game.ingame.GameManager;
import inf112.game.ingame.ShopState;
import inf112.game.ingame.VictoryState;
import inf112.game.interfaces.CombatInterface;
import inf112.game.map.nodes.NodeType;
import inf112.game.menu.EndState;
import inf112.gfx.CameraController;
import inf112.gfx.Canvas;
import inf112.gfx.Container;
import inf112.gfx.Sprite;
import inf112.gfx.ui.Background;
import inf112.gfx.ui.Button;
import inf112.gfx.ui.Label;
import inf112.utils.AudioManager;
import inf112.utils.Vec2;

import java.util.ArrayList;
import java.util.List;

public class CombatState extends AState implements CombatInterface {

    // Using a state manager for internal state management is overkill. A simple
    // enum will do the jobb.
    private enum State {
        PLAYER_TURN, ENEMY_TURN, DRAW_CARDS, DISCARD_CARDS, END_OF_ROUND, END_OF_TURN, SHUFFLE_DECK, HOLD_CARD,
        END_OF_COMBAT
    }

    private State state;
    private StateManager manager;
    private final EventQueue queue;
    private final WaitGroup group;
    private final Deck deck;
    private final EntityPool pool;
    private final GameManager game;
    private final List<ACard> visibleCards; // List of cards that should be drawn at any given time

    private final float DEAL_DELAY = 0.1f; // Delay between dealing and discarding cards (animation)
    private final NodeType battleType; // Describes the type of fight (Elite, Battle and Boss)

    private final float DEFAULT_HAND_DENSITY = 0.5f;
    private float handDensity = DEFAULT_HAND_DENSITY; // How much cards overlap in your hand

    private boolean waiting = false; // Is true when waiting to switch states

    private int selectedCard = -1; // Index of selected card. -1 if no card is selected
    private int prevSelectedCard = -1;
    private ACard playedCard;

    private Background background;
    private final AudioManager audio;

    // UI components
    private IntentIcon intentIcon;
    private HealthBar playerHealthBar;
    private ShieldBar playerShieldBar;
    private HealthBar enemyHealthBar;
    private ShieldBar enemyShieldBar;
    private Button endTurnButton;
    private EnergyIcon energyIcon;
    private Pile drawPileIcon;
    private Pile discardPileIcon;

    public CombatState(GameManager game, NodeType battleType) {
        super();

        audio = AudioManager.getInstance();

        queue = new EventQueue();
        group = new WaitGroup();
        visibleCards = new ArrayList<>();
        pool = new EntityPool(game.player());

        this.game = game;
        this.battleType = battleType;
        deck = game.deck();

        if (battleType == NodeType.BOSS)
            audio.playMusic("boss", true);
        else {
            String[] tracks = { "combat0", "combat1", "combat2", "combat3" };
            audio.playMusic(tracks[(int) (Math.random() * tracks.length)], true);
        }
    }

    @Override
    public void init(StateManager manager) {
        this.manager = manager;

        player().resetEnergy();

        // Shuffle deck and set card positions
        deck.reShuffle();
        for (ACard c : deck.getDrawPile())
            c.pos.set(Config.VIEW_WIDTH, Config.VIEW_HEIGHT);

        // Create end turn button. onClick switches state.
        endTurnButton = new Button(button -> {
            if (!waiting) {
                for (ACard c : hand())
                    if (c.autoPlay())
                        playCard(c);

                queue.add(() -> {
                    awaitSwitchState(State.DISCARD_CARDS);
                    return true;
                });
            }
        }, null, new Vec2(Config.VIEW_WIDTH - 400.f, Config.VIEW_HEIGHT - 500.f), Sprite.END_TURN_BUTTON);

        // Create enemies
        List<Enemy> enemies = EnemyFactory.generateEnemies(game.map().floor(), battleType, game.difficulty());
        game.map().nextFloor();
        assert enemies != null;
        enemies.forEach(enemy -> {
            pool.addEnemy(enemy);
            enemy.pos().set(enemyPos());
        });

        intentIcon = new IntentIcon(Vec2.relative(0.6f, 0.4f));

        int w = Config.VIEW_WIDTH / 8;
        int h = w / 10;

        playerHealthBar = new HealthBar(new Vec2(150, 30), h,
                (float) player().health() / (float) player().maxHealth(), false);
        playerShieldBar = new ShieldBar(new Vec2(150, 70), player().shield(), h, false);
        enemyHealthBar = new HealthBar(new Vec2(850, 750), h,
                (float) currentEnemy().health() / (float) currentEnemy().maxHealth(), false);
        enemyShieldBar = new ShieldBar(new Vec2(850, 800), currentEnemy().shield(), h, false);
        Sprite[] bgs = { Sprite.BG_0, Sprite.BG_1, Sprite.BG_2 };
        background = new Background(bgs[(int) (Math.random() * bgs.length)]);

        energyIcon = new EnergyIcon(new Vec2(100, Config.VIEW_HEIGHT / 1.5f), player().energy());

        // DEBUG
        if (Config.DEBUG) {
            addComponent(new Button(manager, new ShopState(game), new Vec2(500, 50), "SHOP"));
            addComponent(new Button(manager, new ChooseCardState(game), new Vec2(500, 100), "CHOOSE CARD"));
            addComponent(new Button(manager, new VictoryState(game), new Vec2(500, 150), "VICTORY STATE"));
        }

        drawPileIcon = new Pile(Vec2.relative(0.95f, 0.95f));
        discardPileIcon = new Pile(Vec2.relative(0.1f, 0.95f));

        // Switch to starting state
        switchState(State.DRAW_CARDS);
    }

    @Override
    public void update() {
        stage(background);
        intentIcon.set(currentEnemy().intent());

        playerHealthBar.setProgress(player().health(), player().maxHealth());
        playerShieldBar.setWidth(player().shield());
        enemyHealthBar.setProgress(currentEnemy().health(), currentEnemy().maxHealth());
        enemyShieldBar.setWidth(currentEnemy().shield());

        drawPileIcon.set(deck.getDrawPile().size());
        stage(drawPileIcon);
        discardPileIcon.set(deck.getDiscardPile().size());
        stage(discardPileIcon);
        stage(new Label(currentEnemy().text(), new Vec2(925, 30)));

        stage((Container) intentIcon);
        stage((Container) playerHealthBar);
        if (playerShieldBar.width() > 0)
            stage((Container) playerShieldBar);
        stage((Container) enemyHealthBar);
        if (enemyShieldBar.width() > 0)
            stage((Container) enemyShieldBar);

        if (pool.isCombatOver())
            switchState(State.END_OF_COMBAT);

        switch (state) {
            case DRAW_CARDS -> drawCardsState();
            case SHUFFLE_DECK -> shuffleDeckState();
            case PLAYER_TURN -> playerTurnState();
            case DISCARD_CARDS -> discardCardsState();
            case END_OF_TURN -> endOfTurnState();
            case ENEMY_TURN -> enemyTurnState();
            case END_OF_ROUND -> endOfRoundState();
            case HOLD_CARD -> holdCardState();
            case END_OF_COMBAT -> endOfCombatState();
        }

        queue.update();
        group.update();

        for (Enemy c : pool.enemies())
            if (!c.isDead())
                stage(c);

        for (ACard c : visibleCards)
            stage((Container) c);

        energyIcon.set(player().energy());
        stage((Container) energyIcon);

        // Stage hovered card to make it overlap the others
        if (selectedCard != -1 && selectedCard < handSize()) {
            stage((Container) hand().get(selectedCard));
        }

    }

    // Draw cards
    // The player starts with 0 cards in their hand at each round. This first state
    // handles dealing out cards to the player with a nice animation. When the
    // animation is finished it switches to the players turn.
    private void drawCardsState() {
        // Number of cards draw each round
        int DEFAULT_HAND_SIZE = 5;
        if (handSize() < DEFAULT_HAND_SIZE) {
            if (drawCard() == null) {
                switchState(State.SHUFFLE_DECK);
            }
        } else if (!waiting) {
            awaitSwitchState(State.PLAYER_TURN);
        }
    }

    // Shuffle deck
    // When in the draw cards state and the draw pile is empty, it will switch to
    // this state. This simply shuffles the discard pile into the draw pile and,
    // when done, goes back to the draw state to continue drawing.
    private void shuffleDeckState() {
        if (!waiting) {
            deck.reShuffleDiscard();
            deck.getDrawPile().forEach(c -> c.pos.set(Config.VIEW_WIDTH, Config.VIEW_HEIGHT));
            awaitSwitchState(State.DRAW_CARDS);
        }
    }

    // Players turn
    // In the players turn they may play cards and use items. The turn ends when the
    // player manually clicks "end turn".
    private void playerTurnState() {
        stage(endTurnButton);

        if (waiting)
            return;

        selectedCard = -1;

        for (int i = 0; i < handSize(); i++) {
            if (hand().get(i).hovered())
                selectedCard = i;
        }

        for (int i = 0; i < handSize(); i++) {
            ACard c = hand().get(i);
            if (c == playedCard)
                continue;

            Vec2 target = getTargetCardPos(i, handSize());

            // Raise card if selected
            if (selectedCard == i) {
                if (c.clicked() && !c.unplayable())
                    playCard(c);

                c.pos.set(target.x, (float) Config.VIEW_HEIGHT - Config.CARD_H);
                continue;
            }

            // Else calculate horizontal shift to clear space right next to selected card
            calculateHShift(i, c, target);
        }

        if (prevSelectedCard != selectedCard)
            audio.playSfx("bap");
        prevSelectedCard = selectedCard;

        float HOVERED_HAND_DENSITY = 0.7f;
        handDensity = handHovered() ? HOVERED_HAND_DENSITY : DEFAULT_HAND_DENSITY;
    }

    private void calculateHShift(int i, ACard c, Vec2 target) {
        if (selectedCard != -1) {
            int diff = Math.abs(selectedCard - i);
            final int spread = 3;

            if (diff <= spread) {
                int n = spread - diff;
                // float offset = (Config.CARD_W * (1f - HAND_DENSITY)) * (float) n;
                float offset = n * 20.f;
                target.x += i < selectedCard ? -offset : offset;
            }
        }

        c.pos.accelerateTowards(target, 3f);
    }

    // Discard cards
    // This happens after the player ends their turn. The remaining cards in their
    // hand is put in the discard pile. Goes to enemys turn after.
    private void discardCardsState() {
        if (!waiting) {
            hand().forEach(this::discardCard);
            awaitSwitchState(State.END_OF_TURN);
        }
    }

    // End of turn
    // This happens after cards have been discarded and before the enemies turn. If
    // the player has any statuses or abilities that should apply after their turn
    // they happen here.
    private void endOfTurnState() {
        for (AStatus status : player().statuses()) {
            int health = player().health();
            player().applyStatus(status);
            if (player().health() < health) {
                shakeCamera();
            }
        }

        player().purgeStatuses();
        currentEnemy().reset();
        switchState(State.ENEMY_TURN);
    }

    // Enemys turn
    // The enemies each get their turn in a random order. They perform their action
    // and, when complete, lets the next enemy go. Swtiches to end of turn state
    // after last enemy has had their turn.
    private void enemyTurnState() {
        if (waiting)
            return;

        for (Enemy enemy : pool.enemies()) {
            queue.add(() -> {
                enemy.ai().turn(this);
                return true;
            }, 0.5f);

            queue.add(() -> {
                enemy.ai().prepare();
                return true;
            }, 0.5f);
        }

        awaitSwitchState(State.END_OF_ROUND);
    }

    // End of round
    // The end of the round may need to do some extra actions based on cards you
    // played, statuses you have, or other events. These things happen here. When
    // they are done we loop back to draw new cards.
    private void endOfRoundState() {
        for (Enemy enemy : enemies()) {
            for (AStatus status : enemy.statuses()) {
                int health = enemy.health();
                enemy.applyStatus(status);
                if (enemy.health() < health) {
                    shakeEnemy();
                }
            }

            enemy.purgeStatuses();
        }

        queue.add(() -> {
            player().endRound();
            return true;
        });

        awaitSwitchState(State.DRAW_CARDS);
    }

    // End of combat
    // Clean up and move on to either victory or death state.
    private void endOfCombatState() {
        deck.reShuffle();

        deck.getDrawPile().removeIf(ACard::isDebuff);
        deck.getDrawPile().removeIf(ACard::given);

        queue.add(() -> {
            audio.playSfx("gong");
            audio.stopMusic();
            return true;
        });

        queue.add(() -> false, 2f);

        queue.add(() -> {
            if (!player().isDead()) {
                if (battleType == NodeType.BOSS)
                    manager.switchTo(new EndState());
                else
                    manager.switchTo(new VictoryState(game));
            } else
                manager.switchTo(new DeathState());
            return true;
        });

    }

    void startWaiting() {
        waiting = true;
    }

    void stopWaiting() {
        waiting = false;
    }

    boolean handHovered() {
        return selectedCard != -1;
    }

    /**
     * Waits for queue and group to finish before switching states. This allowes
     * animations to finish properly before the next state starts. {@code waiting}
     * is set to true while waiting to switch states.
     * 
     * @param state to switch to
     */
    private void awaitSwitchState(State state) {
        if (waiting)
            return;

        startWaiting();

        queue.add(() -> {
            if (group.done()) {
                switchState(state);
                stopWaiting();
                return true;
            }
            return false;
        });
    }

    /**
     * Immediately switches to new state.
     * 
     * @param state to switch to
     */
    private void switchState(State state) {
        this.state = state;
    }

    private List<ACard> hand() {
        return deck.getHandPile();
    }

    private int handSize() {
        return hand().size();
    }

    /**
     * Draws a card, animating it by moving it towards the hand with acceleration
     * Card is added to deck immediately.
     * 
     * @return card added or null if max cards is reached or draw pile is empty
     */
    private ACard drawCard() {
        // Max number of cards in hand
        int MAX_CARDS = 9;
        if (handSize() >= MAX_CARDS)
            return null;

        ACard card = deck.draw();

        if (card == null)
            return null;

        visibleCards.add(card);

        group.addWithDelay(() -> {
            Vec2 target = getTargetCardPos(hand().indexOf(card), handSize());
            return card.pos.accelerateTowards(target, 1f);
        }, DEAL_DELAY);

        group.add(() -> {
            audio.playSfx("dealCard");
            return true;
        });
        return card;
    }

    /**
     * Discards card from hand. Animates by accelerating towards left side of scree.
     * Adds to group. Card is not actually discarded before the animation finishes.
     * 
     * @param card card to discard
     */
    private void discardCard(ACard card) {
        final Vec2 target = new Vec2(-270, Config.VIEW_HEIGHT);

        group.add(() -> {
            deck.discardCard(card);
            return true;
        });

        group.addWithDelay(() -> {
            audio.playSfx("dealCard");
            return true;
        }, DEAL_DELAY / 8);

        group.addWithDelay(() -> {
            if (card.pos.accelerateTowards(target, 1f)) {
                visibleCards.remove(card);
                return true;
            }
            return false;
        }, DEAL_DELAY / 4);
    }

    /**
     * Gets the target position of the card relative to the other cards in the hand.
     * 
     * @param index    index of card from left to right
     * @param numCards number of cards in hand
     * @return target position
     */
    private Vec2 getTargetCardPos(int index, int numCards) {
        final int cardW = 270;
        final int cardH = 420;
        final float density = handDensity;

        float middle = Config.VIEW_WIDTH / 2.f;
        float width = (numCards - 1) * cardW * density + cardW;
        float offset = cardW * density;

        float x = middle + (offset * index) - (width / 2);
        float y = handHovered() ? Config.VIEW_HEIGHT - cardH / 1.5f : Config.VIEW_HEIGHT - cardH * (2f / 5f);

        return new Vec2(x, y);
    }

    /**
     * Plays the given card and runs all related animations
     * 
     * @param card card to play
     */
    private void playCard(ACard card) {
        if (player().energy() < card.cost())
            return;

        playedCard = card;
        final Vec2 target = new Vec2(200, Config.VIEW_HEIGHT / 2.f - Config.CARD_H / 2.f);

        startWaiting();
        queue.add(group::done);
        queue.add(() -> card.pos.accelerateTowards(target, 2f));

        // Play card
        queue.add(() -> {
            card.play(this);
            player().useEnergy(card.cost());
            stopWaiting();
            return true;
        });

        // Wait for animations to finish
        queue.add(group::done);

        // Discard card
        queue.add(() -> {
            playedCard = null;
            discardCard(card);
            return true;
        });
    }

    private void holdCardState() {
        if (hand().isEmpty()) {
            switchState(State.END_OF_TURN);
            return;
        }
        if (hand().size() == 1 && hand().get(0) instanceof ProcrastinationCard) {
            switchState(State.END_OF_TURN);
            return;
        }

        stage(new Label("Hvilke kort vil du beholde til neste runde?", new Vec2(200, 50)));

        for (int i = 0; i < handSize(); i++)
            if (hand().get(i).hovered())
                selectedCard = i;

        for (int i = 0; i < handSize(); i++) {
            ACard c = hand().get(i);
            Vec2 target = getTargetCardPos(i, handSize());

            if (selectedCard == i) {
                if (c.clicked()) {
                    c.pos.set(100, Config.VIEW_HEIGHT - Config.CARD_H - 100.f);
                    deck.holdCard(c);
                    selectedCard = -1;
                    switchState(State.PLAYER_TURN);
                } else {
                    c.pos.set(target.x, (float) Config.VIEW_HEIGHT - Config.CARD_H);
                }
                continue;
            }

            calculateHShift(i, c, target);
        }
    }

    @Override
    public void clean() {
    }

    public void shakeCamera() {
        final CameraController controller = Canvas.getInstance().camController();
        queue.add(() -> controller.accelerateTowards(new Vec2(-3, 0), 5));
        queue.add(() -> controller.accelerateTowards(new Vec2(3, 0), 5));
        queue.add(() -> controller.accelerateTowards(new Vec2(0, 0), 5));
        queue.add(() -> {
            controller.reset();
            return true;
        });
    }

    private void shakeEnemy() {
        float speed = 7;
        queue.add(() -> currentEnemy().pos().accelerateTowards(enemyPos().sub(-10, 3), speed));
        queue.add(() -> currentEnemy().pos().accelerateTowards(enemyPos().sub(10, 0), speed));
        queue.add(() -> currentEnemy().pos().accelerateTowards(enemyPos().sub(-5, -3), speed));
        queue.add(() -> currentEnemy().pos().accelerateTowards(enemyPos().sub(5, 0), speed));
        queue.add(() -> currentEnemy().pos().accelerateTowards(enemyPos(), 5));
    }

    @Override
    public void damagePlayer(int amount) {
        queue.add(() -> {
            currentEnemy().attack(player(), amount);
            audio.playSfx("hurt");
            return true;
        });

        shakeCamera();
    }

    @Override
    public void shieldEnemy(Enemy enemy, int amount) {
        enemy.addShield(amount);
    }

    @Override
    public void giveCardToPlayerHandNextTurn(ACard card) {
        card.setGiven();
        card.pos.set(Vec2.relative(0.2f, 0.2f));
        queue.add(() -> {
            stage((Container) card);
            return false;
        }, 0.5f);
        queue.add(() -> {
            stage((Container) card);
            return card.pos.accelerateTowards(new Vec2(Config.VIEW_WIDTH, Config.VIEW_HEIGHT), 1);
        });

        queue.add(() -> {
            deck.addToDeck(card);
            return true;
        }, 0.5f);
    }

    @Override
    public Player player() {
        return pool.player();
    }

    @Override
    public Enemy currentEnemy() {
        // TODO: switch to current enemy based on turn start/finish
        return pool.enemies().get(0);
    }

    @Override
    public List<Enemy> enemies() {
        return pool.enemies();
    }

    @Override
    public void damageEnemy(Enemy enemy, int amount) {
        queue.add(() -> {
            player().attack(enemy, amount);
            audio.playSfx("hit");
            return true;
        });

        shakeEnemy();
    }

    @Override
    public void shieldPlayer(int amount) {
        player().addShield(amount);
        audio.playSfx("shield");
    }

    @Override
    public void applyStatus(AEntity entity, AStatus status) {
        entity.addStatus(status);
    }

    @Override
    public Enemy targetEnemy() {
        return pool.enemies().get(0);
    }

    @Override
    public void giveRandomCards(int n) {
        for (int i = 0; i < n; i++) {
            queue.add(() -> {
                drawCard();
                return true;
            });
            queue.add(() -> false, DEAL_DELAY);
        }
    }

    @Override
    public void addEnergy(int amount) {
        player().addEnergy(amount);
        audio.playSfx("energy");
    }

    // Where the enemy should be drawn
    private Vec2 enemyPos() {
        return new Vec2(Config.VIEW_WIDTH / 2.f - currentEnemy().width() / 2,
                Config.VIEW_HEIGHT / 2.f - currentEnemy().height() / 2 - 50);
    }

    @Override
    public void discardNCards(int n) {
        int i = 0;
        for (ACard c : hand()) {
            if (c == playedCard) {
                continue;
            }

            discardCard(c);
            i++;

            if (i == n)
                break;
        }
    }

    @Override
    public void giveGold(int n) {
        player().addGold(n);
        audio.playSfx("coins");
    }

}
