package inf112.game.deck.cards;

import inf112.game.components.EnergizedStatus;
import inf112.game.deck.ACard;
import inf112.game.interfaces.CombatInterface;
import inf112.gfx.Sprite;

public class EnergyDrinkCard extends ACard {

    public static final int COST = 1;

    public EnergyDrinkCard() {
        super(COST, 2, Sprite.ENERGY_DRINK_CARD, "Energy Drink");
    }

    @Override
    public void play(CombatInterface ci) {
        ci.applyStatus(ci.player(), new EnergizedStatus());
    }

    @Override
    public String description() {
        return "Become Energized. +25% damage 2 turn.";
    }
}
