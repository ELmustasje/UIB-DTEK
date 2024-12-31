package INF102.lab5.graph;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.HashSet;
import java.util.Random;
import java.util.Set;

public abstract class GraphTest {

    final int N_VERTICES = 1000;

    Random random = new Random();

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

    public void addEdgeToVerticeNotInGraph(IGraph<Integer> graph) {
        int u = N_VERTICES + 1;
        int v = random.nextInt(N_VERTICES);
        assertThrows(IllegalArgumentException.class, () -> graph.addEdge(u, v),
                "You can't add an edge to a vertice that doesn't exist on the graph.");
        assertThrows(IllegalArgumentException.class, () -> graph.addEdge(v, u),
                "You can't add an edge to a vertice that doesn't exist on the graph.");
    }

    public void addEdgeTest(IGraph<Integer> graph) {
        int u = 0;
        int v = 0;
        for (int i = 0; i < N_VERTICES; i++) {
            u = random.nextInt(N_VERTICES);
            v = random.nextInt(N_VERTICES);
            graph.addEdge(u, v);
            assertTrue(graph.adjacent(u, v), "Vertices " + u + " and " + v + " should have an edge between them.");
            assertTrue(graph.adjacent(v, u),
                    "In an undirected graph " + u + " and " + v + " should have an edge both ways.");
        }
    }

    public void removeEdgeTest(IGraph<Integer> graph) {
        for (int i = 0; i < N_VERTICES; i++) {
            int u = random.nextInt(N_VERTICES);
            int v = random.nextInt(N_VERTICES);
            graph.addEdge(u, v);
            assertTrue(graph.adjacent(u, v), "Vertices " + u + " and " + v + " should have an edge between them.");
            assertTrue(graph.adjacent(v, u),
                    "In an undirected graph " + u + " and " + v + " should have an edge both ways.");
            graph.removeEdge(u, v);
            assertFalse(graph.adjacent(u, v), "Vertices " + u + " and " + v + " should not have an edge between them.");
            assertFalse(graph.adjacent(v, u), "Vertices " + v + " and " + u + " should not have an edge between them.");
        }
    }

    public void getNeighbourhoodTest(IGraph<Integer> graph) {
        int u = random.nextInt(N_VERTICES);
        Set<Integer> randomVertices = new HashSet<>();
        for (int i = 0; i < 100; i++) {
            int v = random.nextInt(N_VERTICES);
            graph.addEdge(u, v);
            randomVertices.add(v);
        }

        for (Integer w : graph.neighbours(u)) {
            if (!randomVertices.contains(w))
                fail("A neighbour of u was not in the list of neigbhours");
        }
    }

    public void addVerticeTest(IGraph<Integer> graph) {
        int extraVertice = N_VERTICES + 1;
        assertEquals(N_VERTICES, graph.size(), "Graph should have " + N_VERTICES + " vertices.");
        assertFalse(graph.hasVertice(extraVertice), "Graph should not have vertice " + extraVertice);
        int u = extraVertice;
        graph.addVertice(u);
        assertTrue(graph.hasVertice(u), "Graph should have vertice " + u);
        assertEquals(extraVertice, graph.size(), "Graph should have " + extraVertice + " vertices.");
    }

    public void duplicateVerticeTest(IGraph<Integer> graph) {
        int vertice = N_VERTICES - 2;
        graph.addEdge(vertice, vertice + 1);
        graph.addVertice(vertice);
        assertTrue(graph.adjacent(vertice, vertice + 1),
                "Trying to add a duplicate vertice should not remove the egdes from the original vertice");

    }

    public void removeVerticeTest(IGraph<Integer> graph) {
        addVerticeTest(graph);
        int u = random.nextInt(N_VERTICES);
        assertNotEquals(N_VERTICES, graph.size(), "Graph should not have " + N_VERTICES + " vertices.");
        graph.removeVertice(u);
        assertFalse(graph.hasVertice(u), "Graph should not have vertice " + u);
        assertEquals(N_VERTICES, graph.size(), "Graph should have " + N_VERTICES + " vertices.");
    }

    public void removeEgdesWithVerticeTest(IGraph<Integer> graph) {
        int u = random.nextInt(N_VERTICES);
        int v = random.nextInt(N_VERTICES);
        int z = random.nextInt(N_VERTICES);
        graph.addEdge(u, v);
        graph.addEdge(u, z);
        assertTrue(graph.neighbours(u).size() >= 1);
        assertThrows(NullPointerException.class, () -> {
            graph.removeVertice(u);
            assertTrue(graph.neighbours(u).size() == 0);
        },
                "Removing a vertice should remove the egdes connencting the vertice");

    }

}
