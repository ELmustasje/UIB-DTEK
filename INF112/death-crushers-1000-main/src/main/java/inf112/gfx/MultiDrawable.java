package inf112.gfx;

import java.util.List;

import inf112.utils.Vec2;

public interface MultiDrawable extends Drawable {
    List<Vec2> positions();
}
