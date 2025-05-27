package inf112.gfx.ui;

import com.badlogic.gdx.graphics.Color;

import inf112.gfx.Anchor;
import inf112.gfx.DrawableType;
import inf112.gfx.FontMetrics;
import inf112.gfx.Interactable;
import inf112.utils.Vec2;

public class Label implements Interactable {

    private final Vec2 pos;
    private String text;
    private final Vec2 size;
    private final Vec2 offset;
    private final int scale;
    private Color color;

    public Label(String text, Vec2 pos) {
        this(text, pos, 1);
    }

    public Label(String text, Vec2 pos, Color color) {
        this(text, pos, 1);
        this.color = color;
    }

    public Label(String text, Vec2 pos, int scale) {
        this(text, pos, scale, Anchor.START);
    }

    public Label(String text, Vec2 pos, int scale, Anchor anchor) {
        this.text = text;
        this.scale = scale;
        size = FontMetrics.measureText(text, scale);
        this.pos = pos;

        if (anchor == Anchor.CENTER) {
            offset = new Vec2(size.x / 2, 0);
        } else if (anchor == Anchor.END) {
            offset = new Vec2(size.x, 0);
        } else {
            offset = new Vec2(0, 0);
        }
    }

    @Override
    public Color color() {
        return color != null ? color : Color.WHITE;
    }

    /**
     * Set the text for the label.
     * 
     * @param text
     */
    public void set(String text) {
        this.text = text;
    }

    public void set(Vec2 pos) {
        this.pos.set(pos);
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
        return pos.sub(offset);
    }

    @Override
    public DrawableType type() {
        return DrawableType.TEXT;
    }

    @Override
    public String text() {
        return text;
    }

    @Override
    public float scale() {
        return scale;
    }

    @Override
    public boolean hover(boolean isHovered) {
        return false;
    }

    @Override
    public void click(boolean isClicked) {
    }

}
