package INF102.lab5.graph;

import java.util.Set;

public interface IGraph<V> {

    /**
     * Number of vertices in the graph
     * @return number of vertices in graph
     */
    public int size();

    /**
     * A way to iterate through all vertices of this graph
     * @return set of graph vertices
     */
    public Iterable<V> vertices();

    /**
     * Add <code>vertice</code> to graph
     * @param vertice
     */
    public void addVertice(V vertice);

    /**
     * Remove <code>vertice</code> from graph
     * @param vertice
     */
    public void removeVertice(V vertice);

    /**
     * Add edge between vertice <code>u</code> and <code>v</code>
     * @param u
     * @param v
     * @throws IllegalArgumentException if the vertices given are not in the graph
     */
    public void addEdge(V u, V v);

    /**
     * Remove edge between vertice <code>u</code> and <code>v</code>
     * @param u
     * @param v
     */
    public void removeEdge(V u, V v);

    /**
     * Checks if the given <code>vertice</code> is in the graph
     * @param vertice
     * @return true if vertice is in graph
     */
    public boolean hasVertice(V vertice);

    /**
     * Checks if two given vertices are adjacent
     *
     * @return true if both vertices are in the graph and there is an edge between
     *         them in the graph
     */
    public boolean adjacent(V u, V v);

    /**
     * Finds all neighbours of vertice <code>u</code>. 
     * The neighbours of a vertice is all vertices which it has an edge to.
     * @param vertice
     * @return list of all neighbours
     */
    public Set<V> neighbours(V vertice);
    
}
