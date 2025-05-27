package inf112.game.ingame.combat;

import java.util.ArrayList;
import java.util.List;

import inf112.gfx.Atlas;
import inf112.gfx.Container;
import inf112.gfx.Drawable;
import inf112.gfx.DrawableType;
import inf112.gfx.Interactable;
import inf112.gfx.Sprite;
import inf112.gfx.ui.StaticSprite;
import inf112.utils.Vec2;

public class EnergyIcon implements Container, Drawable {
    private final Vec2 pos;

    private final int width;
    private final int height;

    private final List<Drawable> drawables;
    private final List<Interactable> interactables;

    int amount;

    // private Label label;

    public EnergyIcon(Vec2 pos, int amount) {
        this.pos = pos;
        this.amount = amount;

        // All icons are the same size
        Vec2 size = Atlas.sizeOf(Sprite.ENERGY);
        width = (int) size.x;
        height = (int) size.y;

        // label = new Label("" + amount + "", pos.cpy(), 1, Anchor.START);

        drawables = new ArrayList<>();
        drawables.add(this);

        interactables = new ArrayList<>();
    }

    public void set(int amount) {
        this.amount = amount;
    }

    @Override
    public List<Drawable> drawables() {
        drawables.clear();
        drawables.add(this);
        drawables.add(new StaticSprite(switch (amount) {
            case 0 -> Sprite.NUM_0;
            case 1 -> Sprite.NUM_1;
            case 2 -> Sprite.NUM_2;
            case 3 -> Sprite.NUM_3;
            case 4 -> Sprite.NUM_4;
            case 5 -> Sprite.NUM_5;
            case 6 -> Sprite.NUM_6;
            case 7 -> Sprite.NUM_7;
            case 8 -> Sprite.NUM_8;
			default -> Sprite.NUM_9;
        }, pos));
        return drawables;
    }

    @Override
    public List<Interactable> interactables() {
        return interactables;
    }

    @Override
    public float width() {
        return width;
    }

    @Override
    public float height() {
        return height;
    }

    @Override
    public Vec2 pos() {
        return pos;
    }

    @Override
    public DrawableType type() {
        return DrawableType.SPRITE;
    }

    @Override
    public Sprite sprite() {
        return Sprite.ENERGY;
    }

}
