package inf112.game.deck.cards;

import inf112.game.deck.ACard;
import inf112.game.interfaces.CombatInterface;
import inf112.gfx.Sprite;
import inf112.utils.AudioManager;

public class GunCard extends ACard {

    public static final int COST = 1;
    private int ammo;

    public GunCard() {
        super(COST, 5, Sprite.GUN_CARD, "Gun");
        ammo = 3;
    }

    @Override
    public void play(CombatInterface ci) {
        ci.damageEnemy(ci.currentEnemy(), 24);
        AudioManager.getInstance().playSfx("gun");
        ammo--;
    }

    @Override
    public String description() {
        return "Deal 24 damage. " + ammo + " bullets left.";
    }

    @Override
    public boolean unplayable() {
        return ammo <= 0;
    }


}
