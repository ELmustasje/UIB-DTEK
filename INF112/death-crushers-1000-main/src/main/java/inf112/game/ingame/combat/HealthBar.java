package inf112.game.ingame.combat;

import inf112.core.Config;
import inf112.gfx.DrawableType;
import inf112.gfx.ui.ProgressBar;
import inf112.utils.Vec2;

public class HealthBar extends ProgressBar {

    float progress;
    private boolean isTest;

    public HealthBar(Vec2 pos, int height, float progress, boolean isTest) {
        super(pos, Config.VIEW_WIDTH / 6, height, "Health:", isTest);
        this.progress = progress;
        this.isTest = isTest;
    }

    public void setProgress(int current, int max) {
        this.progress = (float) current / (float) max;
        if (!isTest)
            this.label.set("HP " + current + "/" + max);

    }

    public float getProgress() {
        return progress;
    }

    @Override
    public DrawableType type() {
        return DrawableType.HEALTH_BAR;
    }
}
