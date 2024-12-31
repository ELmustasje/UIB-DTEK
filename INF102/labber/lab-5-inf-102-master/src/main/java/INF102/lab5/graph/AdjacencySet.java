package INF102.lab5.graph;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class AdjacencySet<V> implements IGraph<V> {

    private Set<V> vertices;
    private Map<V, Set<V>> adjacencyList;

    public AdjacencySet() {
        vertices = new HashSet<>();
        adjacencyList = new HashMap<>();
    }

    @Override
    public int size() {
        return vertices.size();
    }

    @Override
    public Iterable<V> vertices() {
        return Collections.unmodifiableSet(vertices);
    }

    @Override
    public void addVertice(V vertice) {
        if(vertices.contains(vertice)) {
            return;
        }
        adjacencyList.put(vertice, new HashSet<>());
        vertices.add(vertice);
    }

    @Override
    public void removeVertice(V vertice) {
        if(!vertices.contains(vertice)) {
            throw new java.lang.NullPointerException();
        }
        for(V u : vertices){
            removeEdge(vertice,u);
        }
        vertices.remove(vertice);
        adjacencyList.remove(vertice);
    }

    @Override
    public void addEdge(V u, V v) {
        if(!vertices.contains(u) || !vertices.contains(v)){
            throw new IllegalArgumentException();
        }
        Set<V> uAdjacentVertices = adjacencyList.get(u);
        uAdjacentVertices.add(v);
        adjacencyList.put(u,uAdjacentVertices);

        Set<V> vAdjacentVertices = adjacencyList.get(v);
        vAdjacentVertices.add(u);
        adjacencyList.put(v,vAdjacentVertices);
    }

    @Override
    public void removeEdge(V u, V v) {
        if(!adjacencyList.containsKey(u)) {
            return;
        }
        Set<V> uAdjacentVertices = adjacencyList.get(u);
        if(uAdjacentVertices.contains(v)) {
            uAdjacentVertices.remove(v);
            adjacencyList.put(u,uAdjacentVertices);
        }

        Set<V> vAdjacentVertices = adjacencyList.get(v);
        if(vAdjacentVertices.contains(u)) {
            vAdjacentVertices.remove(u);
            adjacencyList.put(v,vAdjacentVertices);
        }
    }

    @Override
    public boolean hasVertice(V vertice) {
        return vertices.contains(vertice);
    }

    @Override
    public boolean adjacent(V u, V v) {
        return adjacencyList.get(u).contains(v);
    }

    @Override
    public Set<V> neighbours(V vertice) {
        return Collections.unmodifiableSet(adjacencyList.get(vertice));
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        for (V vertice : adjacencyList.keySet()) {
            Set<V> verticeList = adjacencyList.get(vertice);

            builder.append(vertice);
            builder.append(" --> ");
            builder.append(verticeList);
            builder.append("\n");
        }
        return builder.toString();
    }

}
