package inf112.game.deck;

import java.util.ArrayList;
import java.util.List;

import inf112.core.Config;
import inf112.game.entity.AEntity;
import inf112.game.interfaces.CombatInterface;
import inf112.gfx.Atlas;
import inf112.gfx.Container;
import inf112.gfx.Drawable;
import inf112.gfx.DrawableType;
import inf112.gfx.Interactable;
import inf112.gfx.Palette;
import inf112.gfx.Sprite;
import inf112.gfx.ui.Label;
import inf112.utils.Vec2;

/**
 * Abstract base class for all cards in the game. Provides common functionality
 * for managing card properties and components
 * This class is quite different from an {@link AEntity}, as it does not require
 * the same fields,
 * nor the same methods.
 */
public abstract class ACard implements Container, Interactable {

    public Vec2 pos;

    protected int price;
    protected int rarity;
    protected int cost;
    protected boolean playable;
    private float scale;
    protected Sprite sprite;
    private final DrawableType TYPE = DrawableType.SPRITE;
    private boolean isClicked;
    private boolean isHovered;

    private final List<Interactable> interactables;
    private final List<Drawable> drawables;

    private Label description;
    private Label nameLabel;
    private boolean given;

    /**
     * Constructs a new card with the specified name, cost, and description.
     *
     * @param cost The cost to play the card.
     */
    public ACard(int cost, int rarity, Sprite sprite, String name) {
        this.cost = cost;
        this.scale = 1;
        this.rarity = rarity;
        this.sprite = sprite;
        pos = new Vec2(0, 0);
        playable = true;
        isClicked = false;
        isHovered = false;
        price = rarity * 12;

        drawables = new ArrayList<>();
        interactables = new ArrayList<>();

        if (!Config.TEST) {
            description = new Label(wrapText(description(), 15), new Vec2(0, 0));
            nameLabel = new Label(name, new Vec2(0, 0), Palette.ORANGE_LIGHT);
        }

        drawables.add(description);
        drawables.add(nameLabel);
        interactables.add(this);
        given = false;
    }

    public void setGiven() {
        given = true;
    }

    /**
     * Returns true if card was given to player during combat, should be removed.
     * 
     * @return if card was given during combat
     */
    public boolean given() {
        return given;
    }


    /**
     * Plays card, should apply all effects and other actions using combat
     * interface.
     * 
     * @param ci interface to interact with combat environment
     */
    public abstract void play(CombatInterface ci);

    /**
     * Gets the cost of playing the card.
     *
     * @return The cost of the card.
     */
    public int cost() {
        return cost;
    }

    /**
     * Gets the price of the card.
     *
     * @return The price of the card.
     */
    public int price() {
        return price;
    }

    /**
     * Gets the rarity of the card.
     *
     * @return The rarity of the card.
     */
    public int rarity() {
        return rarity;
    }


    @Override
    public Sprite sprite() {
        return sprite;
    }

    @Override
    public Vec2 pos() {
        description.set(wrapText(description(), 15));
        description.set(pos.add(40, Config.CARD_H / 2.f + 60));
        nameLabel.set(pos.add(40, Config.CARD_H / 2.f + 30));
        return pos;
    }

    @Override
    public float width() {
        return Atlas.sizeOf(sprite).x;
    }

    @Override
    public float height() {
        return Atlas.sizeOf(sprite).y;
    }

    @Override
    public DrawableType type() {
        return TYPE;
    }

    @Override
    public float scale() {
        return scale;
    }

    public void setScale(float v) {
        scale = v;
    }

    @Override
    public boolean hover(boolean isHovered) {
        this.isHovered = isHovered;
        return false;
    }

    @Override
    public void click(boolean isClicked) {
        this.isClicked = isClicked;
    }

    public boolean clicked() {
        return isClicked;
    }

    public boolean hovered() {
        return isHovered;
    }

    @Override
    public List<Drawable> drawables() {
        return drawables;
    }

    @Override
    public List<Interactable> interactables() {
        return interactables;
    }

    public static String wrapText(String text, int n) {
        if (text == null || n <= 0)
            return text;

        StringBuilder wrappedText = new StringBuilder();
        String[] words = text.split("\\s+"); // Split text into words by whitespace
        StringBuilder line = new StringBuilder();

        for (String word : words) {
            // Check if adding the word exceeds the limit
            if (line.length() + word.length() > n) {
                wrappedText.append(line.toString().trim()).append("\n");
                line.setLength(0); // Reset line
            }
            line.append(word).append(" "); // Add word to current line
        }

        // Append the last line if it contains text
        if (!line.isEmpty()) {
            wrappedText.append(line.toString().trim());
        }

        return wrappedText.toString();
    }

    // Card types, override in child to specify. Defaults to "normal" card.

    /**
     * Is the card unplayable?
     * 
     * @return
     */
    public boolean unplayable() {
        return false;
    }


    /**
     * Should the card play when in your hand?
     * 
     * @return
     */
    public boolean autoPlay() {
        return false;
    }

    /**
     * Is this a negative effect card
     *
     * @return
     */
    public boolean isDebuff() {
        return false;
    }

    /**
     * Get card description.
     * 
     * @return
     */
    public String description() {
        return "";
    }
}
