package INF102.lab5.graph;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Set;

/**
 * This class is used to conduct search algorithms of a graph
 */
public class GraphSearch<V> implements IGraphSearch<V> {

    private IGraph<V> graph;


    public GraphSearch(IGraph<V> graph) {
        this.graph = graph;
    }

    @Override
    public boolean connected(V u, V v) {
        if (u.equals(v)) return true;

        Set<V> visited = new HashSet<>();
        Queue<V> queue = new LinkedList<>();
        queue.add(u);
        visited.add(u);

        while (!queue.isEmpty()) {
            V currentNode = queue.poll();
            for (V neighbor : graph.neighbours(currentNode)) {
                if (!visited.contains(neighbor)) {
                    if (neighbor.equals(v)) return true;
                    queue.add(neighbor);
                    visited.add(neighbor);
                }
            }
        }

        return false;
    }


}
