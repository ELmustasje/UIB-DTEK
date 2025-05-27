package inf112.game.ai;

/**
 * Enum representing different move types an enemy can take.
 */
public enum IntentType {
    ATTACK, // Deals damage to the player
    BUFF, // Applies a buff to self
    DEBUFF, // Applies a negative effect to the player
    SHIELD, // Applies shield to self
    UNKNOWN, // The enemies intention is unknown (not attack)
}
