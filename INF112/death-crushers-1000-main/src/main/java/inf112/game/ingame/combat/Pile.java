package inf112.game.ingame.combat;

import inf112.gfx.Atlas;
import inf112.gfx.DrawableType;
import inf112.gfx.Interactable;
import inf112.gfx.Sprite;
import inf112.utils.Vec2;

public class Pile implements Interactable {

    private final Vec2 pos;
    private final Vec2 size;
    private String txt;

    public Pile(Vec2 pos) {
        this.pos = pos;
        this.size = Atlas.sizeOf(Sprite.NUM_0);
        txt = "0";
    }

    @Override
    public float width() {
        return size.x;
    }

    public void set(int n) {
        txt = "" + n + "";
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
        return DrawableType.TEXT;
    }

    @Override
    public float scale() {
        return 2;
    }

    @Override
    public Sprite sprite() {
        return Sprite.NUM_0;
    }

    @Override
    public String text() {
        return txt;
    }

    @Override
    public boolean hover(boolean isHovered) {
        return true;
    }

    @Override
    public void click(boolean isClicked) {

    }

}
