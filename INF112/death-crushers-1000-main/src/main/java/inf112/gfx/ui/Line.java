package inf112.gfx.ui;

import java.util.ArrayList;
import java.util.List;
import inf112.gfx.DrawableType;
import inf112.gfx.MultiDrawable;
import inf112.gfx.Sprite;
import inf112.utils.Vec2;

public class Line implements MultiDrawable {

    private final Vec2 pos; // Acts as the starting position
    private final Vec2 endPos;
    private final float width;
    private Sprite sprite;

    public Line(Vec2 pos, Vec2 endPos, float width) {
        this.pos = pos;
        this.endPos = endPos;
        this.width = width;
    }

    public Line(Vec2 pos, Vec2 endPos, Sprite sprite) {
        this.pos = pos;
        this.endPos = endPos;
        this.sprite = sprite;
        this.width = 0;
    }

    @Override
    public float width() {
        return width;
    }

    @Override
    public float height() {
        return 0;
    }

    @Override
    public Vec2 pos() {
        return pos;
    }

    @Override
    public DrawableType type() {
        return DrawableType.LINE;
    }

    @Override
    public Sprite sprite() {
        return sprite;
    }

    @Override
    public List<Vec2> positions() {
        float dx = endPos.x - pos.x;
        float dy = endPos.y - pos.y;
        float distance = (float) Math.sqrt(dx * dx + dy * dy);

        int steps = Math.max(1, (int) distance / 20);

        List<Vec2> points = new ArrayList<>();

        for (int i = 0; i <= steps; i++) {
            float t = (float) i / steps;
            float x = pos.x + (endPos.x - pos.x) * t;
            float y = pos.y + (endPos.y - pos.y) * t;
            points.add(new Vec2(x, y));
        }

        return points;
    }
}
