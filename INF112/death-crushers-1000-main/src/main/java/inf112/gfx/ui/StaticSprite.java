package inf112.gfx.ui;

import inf112.gfx.Atlas;
import inf112.gfx.Drawable;
import inf112.gfx.DrawableType;
import inf112.gfx.Sprite;
import inf112.utils.Vec2;

public class StaticSprite implements Drawable {

    private final Sprite sprite;
    private final Vec2 pos;
    private final int w;
    private final int h;

    public StaticSprite(Sprite sprite, Vec2 pos) {
        this.sprite = sprite;
        this.pos = pos;

        Vec2 size = Atlas.sizeOf(sprite);
        w = (int) size.x;
        h = (int) size.y;
    }

    @Override
    public float width() {
        return w;
    }

    @Override
    public float height() {
        return h;
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
        return sprite;
    }

    public void set(Vec2 pos) {
        this.pos.set(pos);
    }

}
