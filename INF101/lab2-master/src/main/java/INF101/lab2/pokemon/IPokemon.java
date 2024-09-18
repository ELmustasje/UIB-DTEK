package INF101.lab2.pokemon;

public interface IPokemon {

    /**
     * Get name of the pokémon
     * @return name of pokémon
     */
    public String getName();

    /**
     * Get strength of pokémon
     * @return strength of pokémon
     */
    public int getStrength();

    /**
     * Get current number of health points of pokémon
     * @return current HP of pokémon
     */
    public int getCurrentHP();

    /**
     * Get maximum number of health points of pokémon
     * @return max HP of pokémon
     */
    public int getMaxHP();

    /**
     * Check if the pokémon is alive. 
     * A pokemon is alive if current HP is higher than 0
     * @return true if current HP > 0, false if not
     */
    public boolean isAlive();

    /**
     * Attack another pokémon. The method conducts an attack by <code>this</code>
     * on <code>target</code>. Calculate the damage using the pokémons strength
     * and a random element. Reduce <code>target</code>s health.
     * 
     * If <code>target</code> has 0 HP then print that it was defeated.
     * 
     * @param target pokémon that is being attacked
     */
    public void attack(IPokemon target);
    
    /**
   * Damage the pokémon. This method reduces the number of
   * health points the pokémon has by <code>damageTaken</code>.
   * If <code>damageTaken</code> is higher than the number of current
   * health points then set current HP to 0.
   *
   * It should not be possible to deal negative damage, i.e. increase the number of health points.
   *
   * The method should print how much HP the pokemon is left with.
   *
   * @param damageTaken
   */
    public void damage(int damageTaken);

}
