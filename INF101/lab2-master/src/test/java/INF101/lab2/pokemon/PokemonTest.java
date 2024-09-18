package INF101.lab2.pokemon;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class PokemonTest {
    
    IPokemon pokemon;
    String name = "Mew";

    @BeforeEach
    public void setup() {
        pokemon = new Pokemon(name);
    }

    @Test
    public void getNameTest() {
        assertEquals(name, pokemon.getName());
    }

    @Test
    public void getStrengthTest() {
        int strength = pokemon.getStrength();
        if (strength == 0)
            fail("Strength cannot be 0.");
    }

    @Test
    public void healthPointsTest() {
        int maxHP = pokemon.getMaxHP();
        assertNotEquals(maxHP, 0, "A pokemon should not have 0 HP as their max.");

        int initialHP = pokemon.getCurrentHP();
        assertEquals(maxHP, initialHP, "Maximum HP and current HP should be equal before engaging in battle.");

        pokemon.damage(1);
        assertNotEquals(maxHP, pokemon.getCurrentHP(), "Maximum HP and current HP should not be equal after taking damage.");
    }

    @Test
    public void damageTest() {
        int currentHP = pokemon.getCurrentHP();
        int damage = 10;
        pokemon.damage(damage);

        assertEquals(currentHP - damage, pokemon.getCurrentHP());
    }

    @Test
    public void cannotInflictNegativeDamage() {
        int currentHP = pokemon.getCurrentHP();
        int damage = -1;
        pokemon.damage(damage);

        assertFalse(currentHP < pokemon.getCurrentHP(), "Inflicting negative damage, i.e. increasing the HP, should not be possible");
    }

    @Test
    public void hpNotBelowZeroTest1() {
        int currentHP = pokemon.getCurrentHP();
        pokemon.damage(currentHP);
        assertEquals(0, pokemon.getCurrentHP());

        pokemon.damage(1);
        assertEquals(0, pokemon.getCurrentHP(), "Damage was should be 0 when health is depleted, never negative HP.");
    }

    @Test
    public void hpNotBelowZeroTest2() {
        int currentHP = pokemon.getCurrentHP();
        pokemon.damage(currentHP + 1);
        assertEquals(0, pokemon.getCurrentHP(), "Damage was should be 0 when health is depleted, never negative HP.");
    }

    @Test
    public void isAliveTest() {
        assertTrue(pokemon.isAlive());
        pokemon.damage(pokemon.getCurrentHP());
        assertFalse(pokemon.isAlive());
    }

    @Test
    public void attackTest() {
        IPokemon target = new Pokemon("MewTwo");
        int targetHP = target.getCurrentHP();
        // Perform an attack 100 times. A pokemon may do 0 damage sometimes, but not 100 times in a row (astronomical chances).
        for (int i = 0; i < 100; i++) { 
            pokemon.attack(target);
            if (pokemon.getCurrentHP() <= 0)
                break;
        }
        assertTrue(targetHP > target.getCurrentHP());
    }

    @Test
    public void attackDefeatTest() {
        IPokemon target = new Pokemon("MewTwo");
        assertTrue(target.isAlive());
        for (int i = 0; i < 1000; i++) { 
            pokemon.attack(target);
        }
        assertFalse(target.isAlive());
        assertTrue(pokemon.isAlive());
    }

    @Test
    public void toStringTest() {
        // Expected: "Mew HP: (??/??) STR: ?" 
        // where the questionmarks differ in value
        String pokemonString = pokemon.toString();
        String[] tokens = pokemonString.split(" ");
        assertEquals(name, tokens[0]);
        assertEquals("HP:", tokens[1]);

        char first = tokens[2].charAt(0);
        char last = tokens[2].charAt(tokens[2].length()-1);
        assertEquals('(', first);
        assertEquals(')', last);
        
        assertEquals("STR:", tokens[3]);
    }

}
