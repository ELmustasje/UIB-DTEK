package INF101.lab2.pokemon;

import java.lang.Math;
import java.util.Random;

public class Pokemon implements IPokemon {
    private String name;
    private int healthPoints;
    private int maxHealthPoints;
    private int strength;
    private Random random;

    public Pokemon(String name){
        this.name = name;
        this.random = new Random();
        this.healthPoints = (int) Math.abs(Math.round(100+10*random.nextGaussian()));
        this.maxHealthPoints = this.healthPoints;
        this.strength = (int) Math.abs(Math.round(20+10*random.nextGaussian()));
    }

    public String getName() {
        return name;
    }

    @Override
    public int getStrength() {
        return strength;
    }

    @Override
    public int getCurrentHP() {
        return healthPoints;
    }

    @Override
    public int getMaxHP() {
        return maxHealthPoints;
    }

    public boolean isAlive() {
        return healthPoints > 0;
    }

    @Override
    public void attack(IPokemon target) {
        System.out.println(name + " attacks " + target.getName());
        int damageInflicted = (int) (this.strength + this.strength / 2 * random.nextGaussian());
        target.damage(damageInflicted);

        if(!target.isAlive()){
            System.out.println(target.getName() + " is defated by " + name);
        }
    }

    @Override
    public void damage(int damageTaken) {
        if(damageTaken < 0){
            return;
        }
        healthPoints -= damageTaken;
        if(healthPoints < 0){
            healthPoints = 0;
        }
        System.out.println(String.format("%s takes %s damange and is left with %s/%s HP",name,damageTaken,healthPoints,maxHealthPoints));

    }

    @Override
    public String toString() {
        return (name + " HP: ("+healthPoints+"/"+maxHealthPoints+") STR: "+ strength);
    }

}
