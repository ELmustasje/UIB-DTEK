package inf112.gfx.ui;

import inf112.gfx.Atlas;
import inf112.gfx.Drawable;
import inf112.gfx.DrawableType;
import inf112.gfx.Sprite;
import inf112.utils.Vec2;

public class Background implements Drawable {

    private final Sprite sprite;
    private final Vec2 pos;
    private final Vec2 size;

    public Background(Sprite sprite) {
        this.sprite = sprite;
        pos = new Vec2(0, 0);
        size = Atlas.sizeOf(sprite);
    }

    @Override
    public float width() {
        return size.x;
    }

    @Override
    public float height() {
        return size.y;
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

}
