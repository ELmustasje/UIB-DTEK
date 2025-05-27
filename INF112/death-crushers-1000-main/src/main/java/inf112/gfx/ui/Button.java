package inf112.gfx.ui;

import com.badlogic.gdx.graphics.Color;

import inf112.core.state.IState;
import inf112.core.state.StateManager;
import inf112.gfx.Atlas;
import inf112.gfx.DrawableType;
import inf112.gfx.FontMetrics;
import inf112.gfx.Interactable;
import inf112.gfx.Palette;
import inf112.gfx.Sprite;
import inf112.utils.Vec2;

public class Button implements Interactable {
    private final Vec2 pos;
    private final String text;
    private final float width;
    private final float height;
    private final ButtonCallback onClick;
    private final ButtonCallback onHover;
    private final Sprite normalSprite;
    private final Sprite hoverSprite;
    private Sprite sprite;

    // Vectors so we can use acceleration to animate
    private Vec2 targetScale;
    private Vec2 scale;

    /**
     * This constructor is called by the other public constructors.
     * This is done to ensure we either utilize text OR sprite, not both.
     * 
     * @param pos
     * @param text
     * @param sprite
     */
    private Button(ButtonCallback onClick, ButtonCallback onHover, Vec2 pos, String text, Sprite normalSprite,
            Sprite hoverSprite) {
        this.pos = pos;
        this.text = text;
        this.normalSprite = normalSprite;
        this.hoverSprite = (hoverSprite == null) ? normalSprite : hoverSprite;
        this.sprite = normalSprite;

        if (normalSprite != Sprite.NONE && normalSprite != null) {
            Vec2 size = Atlas.sizeOf(normalSprite);
            width = size.x;
            height = size.y;
        } else if (text != null) {
            Vec2 size = FontMetrics.measureText(text, 1);
            width = size.x;
            height = size.y;
        } else {
            width = 100;
            height = 100;
        }

        this.onClick = onClick;
        this.onHover = onHover;

        scale = new Vec2(1, 0);
        targetScale = new Vec2(1, 0);
    }

    public Button(ButtonCallback onClick, ButtonCallback onHover, Vec2 pos, String text) {
        this(onClick, onHover, pos, text, Sprite.NONE, Sprite.NONE);
    }

    public Button(ButtonCallback onClick, ButtonCallback onHover, Vec2 pos, Sprite normalSprite, Sprite hoverSprite) {
        this(onClick, onHover, pos, null, normalSprite, hoverSprite);
    }

    public Button(ButtonCallback onClick, ButtonCallback onHover, Vec2 pos, Sprite sprite) {
        this(onClick, onHover, pos, null, sprite, null);
    }

    public Button(StateManager manager, IState state, Vec2 pos, String text) {
        this(button -> {
            if (state != null)
                manager.switchTo(state);
        }, btn -> {
        }, pos, text);
    }

    public Button(StateManager manager, IState state, Vec2 pos, Sprite normalSprite, Sprite hoverSprite) {
        this(button -> {
            if (state != null)
                manager.switchTo(state);
        }, null, pos, normalSprite, hoverSprite);
    }

    /**
     * Temporary constructor to be used until we have a sprite (e.g. MapState)
     */
    public Button(ButtonCallback onClick, ButtonCallback onHover, Vec2 pos, int width, int height) {
        this.pos = pos;
        this.sprite = null;
        this.normalSprite = null;
        this.hoverSprite = null;
        this.height = height;
        this.width = width;
        text = null;
        this.onClick = onClick;
        this.onHover = onHover;
    }

    public Color color() {
        return Palette.FOREGROUND;
    }

    public String text() {
        return text;
    }

    @Override
    public Sprite sprite() {
        return (sprite != null && sprite != Sprite.NONE) ? sprite : Sprite.NONE;
    }

    @Override
    public Vec2 pos() {
        return pos;
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
    public boolean hover(boolean isHovered) {
        if (isHovered) {
            sprite = hoverSprite;
            if (onHover != null) {
                onHover.callback(this);
            }

            targetScale.set(1.1f, 1);
        } else {
            sprite = normalSprite;
            targetScale.set(1f, 0);
        }

        scale.accelerateTowards(targetScale, 3);
        return isHovered;
    }

    @Override
    public void click(boolean isClicked) {
        if (isClicked && onClick != null) {
            onClick.callback(this);
        }
    }

    @Override
    public DrawableType type() {
        return text != null ? DrawableType.TEXT : DrawableType.SPRITE;
    }

    public void setScale(float s) {
        scale.x = s;
    }

    @Override
    public float scale() {
        return scale.x;
    }
}
