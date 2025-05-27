package inf112.game.ingame.combat;

import java.util.ArrayList;
import java.util.List;

import com.badlogic.gdx.utils.TimeUtils;

import inf112.game.ai.Intent;
import inf112.game.ai.IntentType;
import inf112.gfx.Anchor;
import inf112.gfx.Atlas;
import inf112.gfx.Container;
import inf112.gfx.Drawable;
import inf112.gfx.DrawableType;
import inf112.gfx.Interactable;
import inf112.gfx.Sprite;
import inf112.gfx.ui.Label;
import inf112.utils.Vec2;

public class IntentIcon implements Container, Drawable {

    private final Vec2 pos;
    private Intent intent;

    private final int width;
    private final int height;

    private final List<Drawable> drawables;
    private final List<Interactable> interactables;

    private final Label label;

    public IntentIcon(Vec2 pos) {
        this.pos = pos;
        this.intent = null;

        // All icons are the same size
        Vec2 size = Atlas.sizeOf(Sprite.ATTACK_ICON);
        width = (int) size.x;
        height = (int) size.y;

        label = new Label("LABEL NOT SET (BUG)", pos.cpy(), 1, Anchor.START);

        drawables = new ArrayList<>();
        drawables.add(this);
        drawables.add(label);

        interactables = new ArrayList<>();
    }

    /**
     * Update icons intent type.
     * 
     * @param intent
     */
    public void set(Intent intent) {
        this.intent = intent;

        String lbl = "";
        if (intent.type() == IntentType.ATTACK)
            lbl += intent.value();
        if (intent.type() == IntentType.SHIELD)
            lbl += intent.value();

        label.set(lbl);
    }

    @Override
    public Sprite sprite() {
        return switch (this.intent.type()) {
            case ATTACK -> Sprite.ATTACK_ICON;
            case BUFF -> Sprite.BUFF_ICON;
            case DEBUFF -> Sprite.DEBUFF_ICON;
            case SHIELD -> Sprite.SHIELD_ICON;
            case UNKNOWN -> Sprite.UNKNOWN_ICON;
        };
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
        Vec2 newPos = pos.add(0, (float) Math.sin(TimeUtils.millis() * 0.003) * 20);
        label.set(newPos.add(width() / 2, height()));
        return newPos;
    }

    @Override
    public DrawableType type() {
        return DrawableType.SPRITE;
    }

    @Override
    public List<Drawable> drawables() {
        return drawables;
    }

    @Override
    public List<Interactable> interactables() {
        return interactables;
    }

}
