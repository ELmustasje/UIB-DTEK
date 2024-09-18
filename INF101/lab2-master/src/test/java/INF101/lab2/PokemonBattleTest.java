package INF101.lab2;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import INF101.lab2.pokemon.IPokemon;

public class PokemonBattleTest {
    
    static IPokemon pokemon1;
    static IPokemon pokemon2;

    @BeforeAll
    public static void setup() {
        Main.main(null);
        pokemon1 = Main.pokemon1;
        pokemon2 = Main.pokemon2;
    }

    @Test
    public void initializedPokemon() {
        assertNotNull(pokemon1, "The first pokemon variable has not been initialized.");
        assertNotNull(pokemon2, "The second pokemon variable has not been initialized.");
    }

    @Test
    public void pokemonIsDefeated() {
        if (pokemon1.isAlive() && pokemon2.isAlive())
            fail("One of the pokemon must be defeated after a battle.");
    }


    

}
