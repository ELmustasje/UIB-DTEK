package inf112.gfx;

import java.util.List;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.graphics.Color;
import com.badlogic.gdx.graphics.OrthographicCamera;
import com.badlogic.gdx.graphics.Texture;
import com.badlogic.gdx.graphics.g2d.BitmapFont;
import com.badlogic.gdx.graphics.g2d.GlyphLayout;
import com.badlogic.gdx.graphics.g2d.SpriteBatch;
import com.badlogic.gdx.graphics.g2d.TextureRegion;
import com.badlogic.gdx.graphics.glutils.ShapeRenderer;
import com.badlogic.gdx.math.Vector3;
import com.badlogic.gdx.utils.ScreenUtils;
import com.badlogic.gdx.utils.viewport.FitViewport;
import com.badlogic.gdx.utils.viewport.Viewport;

import inf112.core.Config;
import inf112.game.ingame.combat.HealthBar;
import inf112.game.ingame.combat.ShieldBar;
import inf112.gfx.ui.Slider;
import inf112.utils.Vec2;

public class Canvas {

    private static Canvas instance;

    private final int width;
    private final int height;

    private final BitmapFont font;
    private final OrthographicCamera camera;
    private final Viewport viewport;
    private final SpriteBatch batch;
    private final ShapeRenderer shape;

    private final CameraController camController;

    public Canvas(int width, int height) {
        this.width = width;
        this.height = height;
        batch = new SpriteBatch();
        shape = new ShapeRenderer();
        camera = new OrthographicCamera();
        camera.setToOrtho(true);

        viewport = new FitViewport(width, height, camera);
        viewport.apply();

        font = FontMetrics.getDefaultFont();

        camController = new CameraController(camera);

        instance = this;
    }

    /**
     * Draws text on the screen at a specified position, color, and size.
     *
     * @param text     The text to be drawn.
     * @param position The position (x, y) where the text should be drawn.
     * @param color    The color of the text.
     * @param size     The scale size of the text.
     */
    public void drawText(String text, Vec2 position, Color color, float size) {
        drawText(text, position, color, size, Anchor.START);
    }

    /**
     * Draws text on the screen with the specified anchor.
     *
     * @param text     The text to be drawn.
     * @param position The reference position.
     *                 - For CENTER, position.x will be the center of the text.
     *                 - For LEFT, position.x will be the left side.
     *                 - For RIGHT, position.x will be the right side.
     * @param color    The color of the text.
     * @param size     The scale size of the text.
     * @param anchor   The anchor for the text alignment.
     */
    public void drawText(String text, Vec2 position, Color color, float size, Anchor anchor) {
        font.setColor(color);
        font.getData().setScale(size, -size);

        // Use GlyphLayout to measure the width of the text.
        GlyphLayout layout = new GlyphLayout(font, text);
        float textWidth = layout.width;

        // Calculate the x-coordinate based on the anchor.
        float x;

        if (anchor == Anchor.CENTER)
            x = position.x - textWidth / 2;
        else if (anchor == Anchor.END)
            x = position.x - textWidth;
        else
            x = position.x;

        font.draw(batch, text, x, position.y);
    }

    /**
     * Draws texture to position.
     *
     * @param texture
     * @param position
     */
    public void draw(TextureRegion texture, Vec2 position) {
        batch.draw(texture, position.x, position.y);
    }

    /**
     * Draw texture region with specified scale.
     * 
     * @param texture
     * @param pos
     * @param scale
     */
    public void draw(TextureRegion texture, Vec2 pos, float scale) {
        float w = texture.getRegionWidth() * scale;
        float h = texture.getRegionHeight() * scale;
        batch.draw(texture, pos.x, pos.y, w, h);
    }

    /**
     * Draw sprite at position
     * 
     * @param sprite
     * @param pos
     */
    public void draw(Sprite sprite, Vec2 pos) {
        Atlas.draw(this, sprite, pos, 1);
    }

    public void draw(Sprite sprite, Vec2 pos, float scale) {
        Atlas.draw(this, sprite, pos, scale);
    }

    public void draw(Texture texture, float x, float y) {
        batch.draw(texture, x, y);
    }

    /**
     * Draw rectangle to screen.
     * 
     * @param x     from left
     * @param y     from top
     * @param w     width in pixels
     * @param h     height in pixels
     * @param color libGDX {@link Color} enum value
     */
    public void drawRect(float x, float y, float w, float h, Color color) {
        shape.begin(ShapeRenderer.ShapeType.Filled);
        shape.setColor(color);
        shape.rect(x, y, w, h);
        shape.end();
    }

    public void drawLineSprite(Sprite sprite, List<Vec2> positions, float scale) {
        for (Vec2 pos : positions) {
            draw(sprite, pos, scale);
        }
    }

    /** Returns canvas width in pixels. */
    public int width() {
        return width;
    }

    /** Returns canvas height in pixels. */
    public int height() {
        return height;
    }

    /**
     * Render all drawable objects at once.
     * 
     * @param drawables
     */
    public void render(List<Drawable> drawables) {
        beginDraw();

        for (Drawable d : drawables) {
            Vec2 pos = d.pos();

            // Center position around scale
            if (d.scale() != 1f) {
                Vec2 offset = new Vec2(
                        (d.width() - (d.scale() * d.width())) / 2f,
                        (d.height() - (d.scale() * d.height())) / 2f);

                pos = d.pos().add(offset);
            }

            switch (d.type()) {
                case LINE -> drawLineSprite(d.sprite(), ((MultiDrawable) d).positions(), d.scale());
                case SPRITE -> draw(d.sprite(), pos, d.scale());
                case TEXT -> drawText(d.text(), pos, d.color(), d.scale());
                case BOX -> drawRect(d.pos().x, d.pos().y, d.width(), d.height(), Color.RED);
                case HEALTH_BAR -> {
                    batch.end();
                    if (d instanceof HealthBar hb) {
                        drawRect(hb.pos().x, hb.pos().y, hb.width(), hb.height(), Palette.BACKGROUND);
                        drawRect(hb.pos().x, hb.pos().y, hb.width() * hb.getProgress(), hb.height(), Palette.HEALTH);
                    }
                    batch.begin();
                }
                case SHIELD_BAR -> {
                    batch.end();
                    if (d instanceof ShieldBar sb) {
                        drawRect(sb.pos().x, sb.pos().y, sb.width(), sb.height(), Palette.SHIELD);
                    }
                    batch.begin();
                }
                case SLIDER -> {
                    Slider slider = (Slider) d;
                    batch.end();
                    Vec2 sliderPos = slider.getSliderPos();
                    drawRect(sliderPos.x, sliderPos.y, slider.getSliderSize().x, slider.getSliderSize().y, Palette.BUTTON);
                    drawRect(slider.pos().x,slider.pos().y,slider.width(),slider.height(), Palette.SLIDER);
                    batch.begin();
                }
            }
        }

        if (Config.DEBUG)
            drawText("" + Gdx.graphics.getFramesPerSecond() + "", new Vec2(0, 0), Color.WHITE, 2);

        endDraw();

	}

    /** Start drawing to canvas. Call only once every loop. */
    public void beginDraw() {
        ScreenUtils.clear(0.15f, 0.15f, 0.2f, 1f);
        viewport.apply();
        camera.update();
        batch.setProjectionMatrix(camera.combined);
        shape.setProjectionMatrix(camera.combined);
        batch.begin();
    }

    /** End drawing to canvas. Call once after {@link Canvas#beginDraw()}. */
    public void endDraw() {
        batch.end();
    }

    public OrthographicCamera getCamera() {
        return camera;
    }

    public Viewport getViewport() {
        return viewport;
    }

    /**
     * Resize canvas to new width and height.
     * 
     * @param screenWidth
     * @param screenHeight
     */
    public void resize(int screenWidth, int screenHeight) {
        viewport.update(screenWidth, screenHeight, true);
	}

    /** Clean up internal components at end of lifetime. */
    public void dispose() {
        shape.dispose();
        batch.dispose();
    }

    /**
     * 
     * @return
     */
    public Vec2 getMouseWorldPosition() {
        Vector3 screenCoords = new Vector3(Gdx.input.getX(), Gdx.input.getY(), 0);
        camera.unproject(screenCoords);
        return new Vec2(screenCoords.x, screenCoords.y);
    }

    public static Canvas getInstance() {
        if (instance == null) {
            throw new IllegalStateException("Canvas has not been initialized yet");
        }
        return instance;
    }

    public CameraController camController() {
        return camController;
    }

}
