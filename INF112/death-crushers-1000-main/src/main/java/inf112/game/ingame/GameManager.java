package inf112.game.ingame;

import inf112.game.deck.Deck;
import inf112.game.entity.Player;
import inf112.game.map.GameMap;

/**
 * Abstraction over the InGameState class which contains all components such as
 * map, player, deck etc. This interface can be passed to child states if
 * InGameStates to let them have access to game objects.
 */
public interface GameManager {

    /**
     * Get the players deck
     * 
     * @return deck
     */
    Deck deck();

    /**
     * Get the map tree
     * 
     * @return map
     */
    GameMap map();

    /**
     * Get player instance
     * 
     * @return player
     */
    Player player();

    /**
     * A range from 1 to 3 (both inclusive), where 3 is the hardest difficulty.
     * 
     * @return
     */
    int difficulty();
}
