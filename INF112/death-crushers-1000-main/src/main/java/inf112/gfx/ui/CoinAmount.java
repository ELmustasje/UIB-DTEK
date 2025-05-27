package inf112.gfx.ui;

import java.util.ArrayList;
import java.util.List;

import inf112.gfx.Atlas;
import inf112.gfx.Container;
import inf112.gfx.Drawable;
import inf112.gfx.DrawableType;
import inf112.gfx.Interactable;
import inf112.gfx.Sprite;
import inf112.utils.Vec2;

public class CoinAmount implements Container, Drawable {
    private final Vec2 pos;

    private final int width;
    private final int height;

    private final List<Drawable> drawables;
    private final List<Interactable> interactables;

    private final Label label;

    public CoinAmount(Vec2 pos, int amount) {
        this.pos = pos;

        // All icons are the same size
        Vec2 size = Atlas.sizeOf(Sprite.COIN);
        width = (int) size.x;
        height = (int) size.y;

        label = new Label("" + amount + "", pos.cpy());

        drawables = new ArrayList<>();
        drawables.add(this);
        drawables.add(label);

        interactables = new ArrayList<>();
    }

    public void set(int amount) {
        label.set("" + amount + "");
    }

    @Override
    public List<Drawable> drawables() {
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
        label.set(pos.add(width(), height() / 2 - 7));
        return pos;
    }

    @Override
    public DrawableType type() {
        return DrawableType.SPRITE;
    }

    @Override
    public Sprite sprite() {
        return Sprite.COIN;
    }

}
