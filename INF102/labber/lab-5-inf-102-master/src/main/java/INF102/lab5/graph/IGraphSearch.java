package INF102.lab5.graph;

public interface IGraphSearch<V> {
 
    /**
     * Checks if node <code>u</code> and <code>v</code> are connected,
     * i.e. is there a path of edges between <code>u</code> and <code>v</code>
     * @param u
     * @param v
     * @return true if connected, false if not
     */
    public boolean connected(V u, V v);

}
