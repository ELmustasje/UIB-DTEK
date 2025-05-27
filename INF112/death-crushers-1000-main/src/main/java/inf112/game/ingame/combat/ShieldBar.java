package inf112.game.ingame.combat;

import inf112.core.Config;
import inf112.gfx.DrawableType;
import inf112.gfx.ui.ProgressBar;
import inf112.utils.Vec2;

public class ShieldBar extends ProgressBar {

    private boolean isTest;

    public ShieldBar(Vec2 pos, int shield, int height, boolean isTest) {
        super(pos, calculateWidth(shield), height, "Shield:", isTest);
        this.isTest = isTest;
    }

    private static int calculateWidth(int shield) {
        int SHIELD_WIDTH = 10;
        int MAX_SHIELD_WIDTH = Config.VIEW_WIDTH / 6;
        return Math.min(shield * SHIELD_WIDTH, MAX_SHIELD_WIDTH);
    }

    @Override
    public void setWidth(int shield) {
        width = calculateWidth(shield);
        if (!isTest)
            this.label.set("SH " + shield);
    }

    @Override
    public DrawableType type() {
        return DrawableType.SHIELD_BAR;
    }
}
