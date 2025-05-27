package inf112.game.map.nodes;

/**
 * Defines the different types of nodes that can exist in the game map.
 */
public enum NodeType {
    BATTLE,   // Regular battle encounter
    BOSS,     // Boss battle at the end of the map
    SHOP,     // A shop where the player can purchase items
    REST,     // A resting area where the player can heal
    TREASURE, // A treasure room with rewards
    START,    // Starting node
}
