package graph;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class AdjacencySet<V> implements IGraph<V> {

    private Set<V> vertices;
    private Map<V, Set<V>> adjacencySet;

    public AdjacencySet() {
        vertices = new HashSet<>();
        adjacencySet = new HashMap<>();
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
    public void addVertex(V vertex) {
        if (!vertices.contains(vertex)) {
            vertices.add(vertex);
            adjacencySet.put(vertex, new HashSet<>());
        }
    }

    @Override
    public void removeVertex(V vertex) {
        if (hasVertex(vertex)) {
            vertices.remove(vertex);
            adjacencySet.remove(vertex);
            for (V v : vertices) {
                adjacencySet.get(v).remove(vertex);
            }
        } else {
            throw new IllegalArgumentException("Vertex not in graph");
        }
    }

    @Override
    public void addEdge(V u, V v) {
        if (!hasVertex(v) || !hasVertex(u)) {
            throw new IllegalArgumentException("Vertex not in graph");
        }
        if (!adjacent(u, v)) {
            adjacencySet.get(u).add(v);
            adjacencySet.get(v).add(u);
        }
    }

    @Override
    public void removeEdge(V u, V v) {
        if (!hasVertex(v) || !hasVertex(u)) {
            throw new IllegalArgumentException("Vertex not in graph");
        }
        if (adjacent(u, v)) {
            adjacencySet.get(u).remove(v);
            adjacencySet.get(v).remove(u);
        }
    }

    @Override
    public boolean hasVertex(V vertex) {
        return vertices.contains(vertex);
    }

    @Override
    public boolean adjacent(V u, V v) {
        return adjacencySet.get(u).contains(v);
    }

    @Override
    public Set<V> neighbours(V vertex) {
        return Collections.unmodifiableSet(adjacencySet.get(vertex));
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        for (V vertex : adjacencySet.keySet()) {
            Set<V> verticesList = adjacencySet.get(vertex);

            builder.append(vertex);
            builder.append(" --> ");
            builder.append(verticesList);
            builder.append("\n");
        }
        return builder.toString();
    }

}

