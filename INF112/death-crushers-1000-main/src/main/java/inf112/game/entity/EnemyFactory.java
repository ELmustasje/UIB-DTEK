package inf112.game.entity;

import inf112.game.ai.AttackerAI;
import inf112.game.ai.DrunkDadAI;
import inf112.game.ai.EduroamAI;
import inf112.game.ai.EksamensFestAi;
import inf112.game.ai.ExPhilSeminarLeder;
import inf112.game.ai.StunnerAI;
import inf112.game.map.nodes.NodeType;
import inf112.gfx.Sprite;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class EnemyFactory extends AEnemyFactory {

    /**
     * Creates a list of enemies adjusted in difficulty for the given floor.
     * 
     * @param floor current floor level
     * @param type  node type: BATTLE, ELITE, BOSS
     * @return list of one or more enemies
     */
    public static List<Enemy> generateEnemies(int floor, NodeType type, int difficulty) {
        List<Enemy> enemies;

        switch (type) {
            case BATTLE:
                enemies = generateBattleFloor(floor);
                break;
            case BOSS:
                enemies = generateBossFloor(difficulty);
                break;
            default:
                return null;
        }
        return enemies;
    }

    private static List<Enemy> generateBattleFloor(int floor) {
        List<Enemy> enemies = new ArrayList<>();
        spawners = new ArrayList<>();

        spawners.add(() -> new Enemy(40 + floor * 12, 0, "Eksamensfest", new EksamensFestAi(), Sprite.PARTY_ENEMY));
        spawners.add(() -> new Enemy(25 + floor * 10, 0, "Drunk Dad", new DrunkDadAI(), Sprite.DRUNK_ENEMY));
        spawners.add(() -> new Enemy(25 + floor * 5, 0, "Tung Tung Tung Sahur", new AttackerAI(),
                Sprite.ATTACKER_ENEMY));
        spawners.add(() -> new Enemy(30 + floor * 15, 0, "Exphil Seminarleder", new ExPhilSeminarLeder(),
                Sprite.EXPHIL_ENEMY));
        spawners.add(() -> new Enemy(20 + floor * 5, 0, "La Polizia", new StunnerAI(),
                Sprite.POLICE_ENEMY));

        System.out.println("battle floor");

        Enemy chosen = spawners.get(random.nextInt(spawners.size())).spawn();
        if (lastEnemy != null && Objects.equals(chosen.name, lastEnemy.name)) {
            return generateBattleFloor(floor);
        }

        enemies.add(chosen);
        lastEnemy = chosen;
        return enemies;
    }


    private static List<Enemy> generateBossFloor(int difficulty) {
        List<Enemy> enemies = new ArrayList<>();
        enemies.add(new Enemy(140 * difficulty, 0, "Eduoram", new EduroamAI(), Sprite.EDUROAM_ENEMY));
        return enemies;
    }
}
