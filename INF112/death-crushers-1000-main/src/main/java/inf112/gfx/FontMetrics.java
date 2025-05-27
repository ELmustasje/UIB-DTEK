package inf112.gfx;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.graphics.g2d.BitmapFont;
import com.badlogic.gdx.graphics.g2d.GlyphLayout;
import com.badlogic.gdx.graphics.g2d.freetype.FreeTypeFontGenerator;

import inf112.utils.Vec2;

public class FontMetrics {
    private static final BitmapFont DEFAULT_FONT;
	private static final String FONT_PATH = "font/Montserrat.ttf";

    static {
        FreeTypeFontGenerator generator = new FreeTypeFontGenerator(Gdx.files.internal(FONT_PATH));
        FreeTypeFontGenerator.FreeTypeFontParameter parameter = new FreeTypeFontGenerator.FreeTypeFontParameter();
        parameter.size = 20;
        DEFAULT_FONT = generator.generateFont(parameter);
        generator.dispose();
    }

    /**
     * Measures the dimensions of the given text using the default font.
     * 
     * @param text  The text to measure.
     * @param scale The scale factor to apply.
     * @return A Vec2 where x is the width and y is the height.
     */
    public static Vec2 measureText(String text, float scale) {
        GlyphLayout gl = new GlyphLayout(DEFAULT_FONT, text);
        float width = gl.width * scale;
        float height = DEFAULT_FONT.getLineHeight() * scale;
        if (height < 0) // Idk why, but height is negative sometimes
            height *= -1;
        return new Vec2(width, height);
    }

    public static BitmapFont getDefaultFont() {
        return DEFAULT_FONT;
    }
}
