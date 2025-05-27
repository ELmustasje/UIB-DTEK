package inf112.gfx;

import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.g2d.TextureRegion;

import inf112.game.deck.CardType;
import inf112.game.deck.cards.BeerCard;
import inf112.game.deck.cards.BunkerCard;
import inf112.game.deck.cards.CoffeeBreakCard;
import inf112.game.deck.cards.EnergyDrinkCard;
import inf112.game.deck.cards.GunCard;
import inf112.game.deck.cards.LectureCard;
import inf112.game.deck.cards.NoSignalCard;
import inf112.game.deck.cards.PizzaCard;
import inf112.game.deck.cards.ProcrastinationCard;
import inf112.game.deck.cards.RageCard;
import inf112.game.deck.cards.ReadCard;
import inf112.game.deck.cards.RunTestCard;
import inf112.game.deck.cards.ShieldCard;
import inf112.game.deck.cards.SickCard;
import inf112.game.deck.cards.SkipLectureCard;
import inf112.game.deck.cards.StipendCard;
import inf112.game.deck.cards.StrikeCard;
import inf112.game.deck.cards.StunCard;
import inf112.game.deck.cards.TikTokBreakCard;
import inf112.game.deck.cards.WaffleThursdayCard;
import inf112.utils.Vec2;

import java.util.HashMap;
import java.util.Map;

public class Atlas {

    private static final int T = 16; // Tile size
    public static final float SCALE = 2;

    private static Texture atlas;
    private static final Map<Sprite, TextureRegion> sprites = new HashMap<>();
    private static final Map<Sprite, Sprite[]> combined = new HashMap<>();

    /**
     * Load all sprites and combinations into memory.
     */
    public static void loadSprites(String path) {
        atlas = new Texture(path);

        int i = 8;
        newSprite(Sprite.CARD_TYPE_DEFEND, 0, 0, 8, 13);
        newSprite(Sprite.CARD_TYPE_STATUS, i, 0, 8, 13);
        newSprite(Sprite.CARD_TYPE_ATTACK, i * 2, 0, 8, 13);

        newSprite(Sprite.CARD_COST_0, i * 3, 0, 8, 13);
        newSprite(Sprite.CARD_COST_1, i * 4, 0, 8, 13);
        newSprite(Sprite.CARD_COST_2, i * 5, 0, 8, 13);
        newSprite(Sprite.CARD_COST_3, i * 6, 0, 8, 13);
        newSprite(Sprite.CARD_COST_4, i * 7, 0, 8, 13);

        newSprite(Sprite.ENEMY, 0, 30, 7, 15);

        newSprite(Sprite.ATTACKER_ENEMY, 0, 30, 7, 15);
        newSprite(Sprite.EDUROAM_ENEMY, 8, 30, 13, 15);
        newSprite(Sprite.PARTY_ENEMY, 21, 30, 16, 15);
        newSprite(Sprite.DRUNK_ENEMY, 37, 30, 8, 15);
        newSprite(Sprite.POLICE_ENEMY, 45, 30, 7, 15);
        newSprite(Sprite.EXPHIL_ENEMY, 52, 30, 10, 15);

        newSprite(Sprite.END_TURN_BUTTON, 0, 13, 8, 4);

        i = 2;
        newSprite(Sprite.ATTACK_ICON, 0, 17, 2, 2);
        newSprite(Sprite.UNKNOWN_ICON, i, 17, 2, 2);
        newSprite(Sprite.DEBUFF_ICON, i * 2, 17, 2, 2);
        newSprite(Sprite.SHIELD_ICON, i * 3, 17, 2, 2);
        newSprite(Sprite.BUFF_ICON, i * 4, 17, 2, 2);

        newSprite(Sprite.COIN, i * 5, 17, 2, 2);

        i = 4;
        newSprite(Sprite.MAP_COMBAT, 0, 19, 4, 4);
        newSprite(Sprite.MAP_SHOP, i, 19, 4, 4);
        newSprite(Sprite.MAP_REST, i * 2, 19, 4, 4);
        newSprite(Sprite.MAP_BOSS, i * 3, 19, 4, 4);
        newSprite(Sprite.MAP_START, i * 4, 19, 4, 4);
        newSprite(Sprite.MAP_CARDS, i * 5, 19, 4, 4);

        newSprite(Sprite.MAP_DOT, 0, 23, 1, 1);

        i = 5;
        newSprite(Sprite.ENERGY, 0, 24, 5, 5);
        newSprite(Sprite.NUM_0, i, 24, 5, 5);
        newSprite(Sprite.NUM_1, 2 * i, 24, 5, 5);
        newSprite(Sprite.NUM_2, 3 * i, 24, 5, 5);
        newSprite(Sprite.NUM_3, 4 * i, 24, 5, 5);
        newSprite(Sprite.NUM_4, 5 * i, 24, 5, 5);
        newSprite(Sprite.NUM_5, 6 * i, 24, 5, 5);
        newSprite(Sprite.NUM_6, 7 * i, 24, 5, 5);
        newSprite(Sprite.NUM_7, 8 * i, 24, 5, 5);
        newSprite(Sprite.NUM_8, 9 * i, 24, 5, 5);
        newSprite(Sprite.NUM_9, 10 * i, 24, 5, 5);

        newBackground(Sprite.BG_0, "backgrounds/bg0.jpg");
        newBackground(Sprite.BG_1, "backgrounds/bg1.jpg");
        newBackground(Sprite.BG_2, "backgrounds/bg2.png");

        newBackground(Sprite.BG_REST, "backgrounds/rest.png");

        i = 0;
        createCard(Sprite.PIZZA_CARD, Sprite.PIZZA_BG, CardType.STATUS, PizzaCard.COST, i++);
        createCard(Sprite.TIKTOK_BREAK_CARD, Sprite.TIKTOK_BREAK_BG, CardType.DEFEND, TikTokBreakCard.COST, i++);
        createCard(Sprite.BEER_CARD, Sprite.BEER_BG, CardType.STATUS, BeerCard.COST, i++);
        createCard(Sprite.STUN_CARD, Sprite.STUN_BG, CardType.STATUS, StunCard.COST, i++);
        createCard(Sprite.WAFFLE_THURSDAY_CARD, Sprite.WAFFLE_THURSDAY_BG, CardType.DEFEND, WaffleThursdayCard.COST,
                i++);
        createCard(Sprite.LECTURE_CARD, Sprite.LECTURE_BG, CardType.STATUS, LectureCard.COST, i++);
        createCard(Sprite.PROCRASTINATION_CARD, Sprite.PROCRASTINATION_BG, CardType.STATUS, ProcrastinationCard.COST,
                i++);
        createCard(Sprite.READ_CARD, Sprite.READ_BG, CardType.STATUS, ReadCard.COST, i++);
        createCard(Sprite.RUN_TEST_CARD, Sprite.RUN_TEST_BG, CardType.ATTACK, RunTestCard.COST, i++);
        createCard(Sprite.SHIELD_CARD, Sprite.SHIELD_BG, CardType.DEFEND, ShieldCard.COST, i++);
        createCard(Sprite.STRIKE_CARD, Sprite.STRIKE_BG, CardType.ATTACK, StrikeCard.COST, i++);
        createCard(Sprite.ENERGY_DRINK_CARD, Sprite.ENERGY_DRINK_BG, CardType.STATUS, EnergyDrinkCard.COST, i++);
        createCard(Sprite.SKIP_LECTURE_CARD, Sprite.SKIP_LECTURE_BG, CardType.ATTACK, SkipLectureCard.COST, i++);
        createCard(Sprite.SICK_CARD, Sprite.SICK_BG, CardType.ATTACK, SickCard.COST, i++);
        createCard(Sprite.BUNKER_CARD, Sprite.BUNKER_BG, CardType.DEFEND, BunkerCard.COST, i++);
        createCard(Sprite.COFFEE_BREAK_CARD, Sprite.COFFEE_BREAK_BG, CardType.STATUS, CoffeeBreakCard.COST, i++);
        createCard(Sprite.STIPEND_CARD, Sprite.STIPEND_BG, CardType.STATUS, StipendCard.COST, i++);
        createCard(Sprite.GUN_CARD, Sprite.GUN_BG, CardType.ATTACK, GunCard.COST, i++);
        createCard(Sprite.RAGE_CARD, Sprite.RAGE_BG, CardType.ATTACK, RageCard.COST, i++);
        createCard(Sprite.NO_SIGNAL_CARD, Sprite.NO_SIGNAL_BG, CardType.STATUS, NoSignalCard.COST, i++);
    }

    /**
     * Draw sprite at pos. Atlas figures out internally if the sprite refers to a
     * raw sprite or a combined sprite.
     * 
     * @param canvas canvas to draw to
     * @param sprite sprite to draw
     * @param pos    where to draw the sprite
     * @throws IllegalArgumentException if sprite enum has no mapped texture
     */
    public static void draw(Canvas canvas, Sprite sprite, Vec2 pos, float scale) {
        if (sprites.containsKey(sprite)) {
            canvas.draw(sprites.get(sprite), pos, SCALE * scale);
        }

        // If combined sprite, loop over each sprite and draw sperately at same pos
        else if (combined.containsKey(sprite)) {
            Sprite[] list = combined.get(sprite);

			for (Sprite value : list) {
				TextureRegion texture = sprites.get(value);
				canvas.draw(texture, pos, SCALE);
			}
        }

        else
            throw new IllegalArgumentException("no sprite mapped to value " + sprite);
    }

    /**
     * All coord and size params are in tile sizes
     * 
     * @param sprite
     * @param x
     * @param y
     * @param w
     * @param h
     */
    private static void newSprite(Sprite sprite, int x, int y, int w, int h) {
        TextureRegion region = new TextureRegion(atlas, x * T, y * T, w * T, h * T);
        region.flip(false, true);
        sprites.put(sprite, region);
    }

    /**
     * Creates a new sprite with the combined sprite textures. The new sprite cannot
     * share a name with any other sprite.
     * 
     * @param sprite     new sprite to assign to
     * @param spriteList list of sprites to combine, in order, from back to front
     * @throws IllegalArgumentException if a sprite enum in the list has no mapped
     *                                  texture
     */
    private static void newCombined(Sprite sprite, Sprite[] spriteList) {
		for (Sprite value : spriteList) {
			if (!sprites.containsKey(value))
				throw new IllegalArgumentException("no sprite is matched to value " + value);
		}

        combined.put(sprite, spriteList);
    }

    private static void createCard(Sprite sprite, Sprite bgSprite, CardType type, int cost, int idx) {
        newSprite(bgSprite, 8 * (idx + 3), 13, 8, 13);

        Sprite typeSprite = switch (type) {
            case ATTACK -> Sprite.CARD_TYPE_ATTACK;
            case DEFEND -> Sprite.CARD_TYPE_DEFEND;
            case STATUS -> Sprite.CARD_TYPE_STATUS;
        };

        Sprite costSprite = switch (cost) {
            case 0 -> Sprite.CARD_COST_0;
            case 1 -> Sprite.CARD_COST_1;
            case 2 -> Sprite.CARD_COST_2;
            case 3 -> Sprite.CARD_COST_3;
            case 4 -> Sprite.CARD_COST_4;
            default -> throw new IllegalArgumentException("invalid card cost");
        };

        newCombined(sprite, new Sprite[] { bgSprite, typeSprite, costSprite });
    }

    /**
     * Make new background sprite
     * 
     * @param sprite
     * @param path
     */
    private static void newBackground(Sprite sprite, String path) {
        sprites.put(sprite, new TextureRegion(new Texture(path)));
    }

    /**
     * Get the width and height of a sprite.
     * 
     * @param sprite
     * @return vector with x as width and y as height
     * @throws IllegalArgumentException if sprite has no texture
     */
    public static Vec2 sizeOf(Sprite sprite) {
        if (!sprites.containsKey(sprite) && !combined.containsKey(sprite)) {
            throw new IllegalArgumentException("sprite enum has no assigned texture, " + sprite);
        }

        if (combined.containsKey(sprite)) {
            // Combined sprite should always layer identically sized sprites
            TextureRegion reg = sprites.get(combined.get(sprite)[0]);
            return new Vec2(reg.getRegionWidth() * SCALE, reg.getRegionHeight() * SCALE);
        }

        TextureRegion reg = sprites.get(sprite);
        return new Vec2(reg.getRegionWidth() * SCALE, reg.getRegionHeight() * SCALE);
    }
}
