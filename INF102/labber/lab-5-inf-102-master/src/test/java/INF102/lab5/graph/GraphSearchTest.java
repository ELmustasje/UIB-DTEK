package INF102.lab5.graph;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Random;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class GraphSearchTest {

    private IGraph<Integer> graph;
    private IGraphSearch<Integer> graphSearch;

    private final int N_VERTICES = 1000;
    private Random random = new Random();

    /**
     * Adds edges of all vertices from 0 to N, i.e.:
     * (0 -> 1), (1 -> 2), ..., (N-1 -> N)
     * 
     * @param graph
     */
    public void createConnectedGraph(IGraph<Integer> graph) {
        for (int i = 0; i < N_VERTICES - 1; i++) {
            graph.addEdge(i, i + 1);
        }
    }

    @BeforeEach
    public void setup() {
        graph = new AdjacencySet<>();
        for (int i = 0; i < N_VERTICES; i++) {
            graph.addVertice(i);
        }
        graphSearch = new GraphSearch<>(graph);
    }
    
    @Test
    public void connectedTest() {
        createConnectedGraph(graph);
        int u = 0;
        int v = N_VERTICES - 1;

        assertTrue(graphSearch.connected(u, v));
    }

    @Test
    public void notConnectedTest() {
        createConnectedGraph(graph);
        int u = 0;
        int v = N_VERTICES - 1;
        int w = N_VERTICES - 2;
        graph.removeEdge(w, v);

        assertFalse(graphSearch.connected(u, v));
    }

    @Test
    public void neighbouringVerticesAreConnectedTest() {
        createConnectedGraph(graph);
        for (int i = 0; i < 1000; i++) {
            int u = random.nextInt(N_VERTICES);
            int v = random.nextInt(N_VERTICES);
            graph.addEdge(u, v);

            Boolean test = graphSearch.connected(u,v);
            assertTrue(graphSearch.connected(u, v));
        }
    }

    @Test
    public void connectedUndirectedTest() {
        for (int i = 0; i < 1000; i++) {
            int u = random.nextInt(N_VERTICES);
            int v = random.nextInt(N_VERTICES);
            graph.addEdge(u, v);
        }

        for (int i = 0; i < 10000; i++) {
            int u = random.nextInt(N_VERTICES);
            int v = random.nextInt(N_VERTICES);
            boolean uConnectedToV = graphSearch.connected(u, v);
            boolean vConnectedToU = graphSearch.connected(v, u);

            assertEquals(uConnectedToV, vConnectedToU);
        }
    }
}
