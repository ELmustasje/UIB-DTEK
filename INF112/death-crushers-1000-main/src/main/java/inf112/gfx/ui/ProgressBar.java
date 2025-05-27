package inf112.gfx.ui;

import java.util.ArrayList;
import java.util.List;

import inf112.gfx.Container;
import inf112.gfx.Drawable;
import inf112.gfx.DrawableType;
import inf112.gfx.Interactable;
import inf112.utils.Vec2;

public abstract class ProgressBar implements Container, Drawable {
    protected Vec2 pos;
    protected int width;
    protected int height;
    protected List<Drawable> drawables;
    protected List<Interactable> interactables;
    protected Label label;

    public ProgressBar(Vec2 pos, int width, int height, String labelText, boolean test) {
        this.pos = pos;
        this.width = width;
        this.height = height;

        drawables = new ArrayList<>();
        interactables = new ArrayList<>();

        drawables.add(this);

        if (test)
            return;

        label = new Label(labelText, new Vec2(pos.x - 125, pos.y + 5));
        drawables.add(label);
    }

    public void setWidth(int width) {
        this.width = width;
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
    public List<Drawable> drawables() {
        return drawables;
    }

    @Override
    public List<Interactable> interactables() {
        return interactables;
    }

    @Override
    public abstract DrawableType type();
}
