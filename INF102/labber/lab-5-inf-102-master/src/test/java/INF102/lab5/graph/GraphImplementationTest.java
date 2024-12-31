package INF102.lab5.graph;


import org.junit.jupiter.api.Test;

/**
 * An implementation of a graph datastrucutre using adjacency matrix should yield a OutOfMemoryException when the number of vertices
 * is too high, while using adjacency list/set should not.
 */
public class GraphImplementationTest {
    
    final int OUT_OF_MEMORY_GRAPH_SIZE = 100000;

    @Test
    public void listNotOutOfMemoryTest() {
        AdjacencySet<Integer> set = new AdjacencySet<Integer>();
        for (int i = 0; i < OUT_OF_MEMORY_GRAPH_SIZE; i++) {
            set.addVertice(i);
        }
    }
}
