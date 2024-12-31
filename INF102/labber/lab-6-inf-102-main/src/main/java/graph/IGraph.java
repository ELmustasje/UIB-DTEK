package graph;

import java.util.Set;

public interface IGraph<V> {

    /**
     * Number of vertices in the graph
     * 
     * @return number of vertices in graph
     */
    public int size();

    /**
     * A way to iterate through all vertices of this graph
     * 
     * @return set of graph vertices
     */
    public Iterable<V> vertices();

    /**
     * Add <code>vertex</code> to graph
     * 
     * @param vertex
     */
    public void addVertex(V vertex);

    /**
     * Remove <code>vertex</code> from graph
     * 
     * @param vertex
     */
    public void removeVertex(V vertex);

    /**
     * Add edge between vertex <code>u</code> and <code>v</code>
     * 
     * @param u
     * @param v
     * @throws IllegalArgumentException if the vertices given are not in the graph
     */
    public void addEdge(V u, V v);

    /**
     * Remove edge between vertex <code>u</code> and <code>v</code>
     * 
     * @param u
     * @param v
     */
    public void removeEdge(V u, V v);

    /**
     * Checks if the given <code>vertex</code> is in the graph
     * 
     * @param vertex
     * @return true if vertex is in graph
     */
    public boolean hasVertex(V vertex);

    /**
     * Checks if two given vertices are adjacent
     *
     * @return true if both vertices are in the graph and there is an edge between
     *         them in the graph
     */
    public boolean adjacent(V u, V v);

    /**
     * Finds all neighbours of vertex <code>u</code>.
     * The neighbours of a vertex is all vertices which it has an edge to.
     * 
     * @param vertex
     * @return list of all neighbours
     */
    public Set<V> neighbours(V vertex);

}
