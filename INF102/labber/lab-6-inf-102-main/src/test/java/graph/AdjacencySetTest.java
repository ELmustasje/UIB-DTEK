package graph;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.HashSet;
import java.util.Random;
import java.util.Set;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class AdjacencySetTest {

IGraph<Integer> graph;

    @BeforeEach
    public void setup() {
        graph = new AdjacencySet<>();
    }

    final int N_VERTICES = 1000;

    Random random = new Random();

    /**
     * Adds edges of all vertices from 0 to N, i.e.:
     * (0 -> 1), (1 -> 2), ..., (N-1 -> N)
     * 
     * @param graph
     */
    public void createConnectedGraph(IGraph<Integer> graph) {
        for (int i = 0; i < N_VERTICES; i++) {
            graph.addVertex(i);
        }
        for (int i = 0; i < N_VERTICES - 1; i++) {
            graph.addEdge(i, i + 1);
        }
    }

    @Test
    public void addEdgeToVertexNotInGraph() {
        int u = N_VERTICES + 1;
        int v = random.nextInt(N_VERTICES);
        assertThrows(IllegalArgumentException.class, () -> graph.addEdge(u, v),
                "You can't add an edge to a vertex that doesn't exist on the graph.");
        assertThrows(IllegalArgumentException.class, () -> graph.addEdge(v, u),
                "You can't add an edge to a vertex that doesn't exist on the graph.");
    }

    @Test
    public void addEdgeTest() {
        int u = 0;
        int v = 0;
        for (int i = 0; i < N_VERTICES; i++) {
            u = random.nextInt(N_VERTICES);
            v = random.nextInt(N_VERTICES);
            graph.addVertex(u);
            graph.addVertex(v);

            graph.addEdge(u, v);
            assertTrue(graph.adjacent(u, v), "Vertices " + u + " and " + v + " should have an edge between them.");
            assertTrue(graph.adjacent(v, u),
                    "In an undirected graph " + u + " and " + v + " should have an edge both ways.");
        }
    }

    @Test
    public void removeEdgeTest() {
        for (int i = 0; i < N_VERTICES; i++) {
            int u = random.nextInt(N_VERTICES);
            int v = random.nextInt(N_VERTICES);
            graph.addVertex(u);
            graph.addVertex(v);
            graph.addEdge(u, v);

            assertTrue(graph.adjacent(u, v), "Vertices " + u + " and " + v + " should have an edge between them.");
            assertTrue(graph.adjacent(v, u),
                    "In an undirected graph " + u + " and " + v + " should have an edge both ways.");
            graph.removeEdge(u, v);
            assertFalse(graph.adjacent(u, v), "Vertices " + u + " and " + v + " should not have an edge between them.");
            assertFalse(graph.adjacent(v, u), "Vertices " + v + " and " + u + " should not have an edge between them.");
        }
    }

    @Test
    public void getNeighbourhoodTest() {
        int u = random.nextInt(N_VERTICES);
        graph.addVertex(u);
        Set<Integer> randomVertices = new HashSet<>();
        for (int i = 0; i < 100; i++) {
            int v = random.nextInt(N_VERTICES);
            graph.addVertex(v);
            graph.addEdge(u, v);
            randomVertices.add(v);
        }

        for (Integer w : graph.neighbours(u)) {
            if (!randomVertices.contains(w))
                fail("A neighbour of u was not in the list of neigbhours");
        }
    }

    @Test
    public void addVertexTest() {
        createConnectedGraph(graph);

        int extraVertex = N_VERTICES + 1;
        assertEquals(N_VERTICES, graph.size(), "Graph should have " + N_VERTICES + " vertices.");
        assertFalse(graph.hasVertex(extraVertex), "Graph should not have vertex " + extraVertex);
        int u = extraVertex;

        graph.addVertex(u);
        assertTrue(graph.hasVertex(u), "Graph should have vertex " + u);
        assertEquals(extraVertex, graph.size(), "Graph should have " + extraVertex + " vertices.");
    }

    @Test
    public void duplicateVertexTest() {
        createConnectedGraph(graph);

        int vertex = N_VERTICES - 2;
        graph.addEdge(vertex, vertex + 1);
        graph.addVertex(vertex);
        assertTrue(graph.adjacent(vertex, vertex + 1),
                "Trying to add a duplicate vertex should not remove the egdes from the original vertex");

    }

    @Test
    public void removeVertexTest() {
        addVertexTest();
        int u = random.nextInt(N_VERTICES);
        assertNotEquals(N_VERTICES, graph.size(), "Graph should not have " + N_VERTICES + " vertices.");
        graph.removeVertex(u);
        assertFalse(graph.hasVertex(u), "Graph should not have vertex " + u);
        assertEquals(N_VERTICES, graph.size(), "Graph should have " + N_VERTICES + " vertices.");
    }

    @Test
    public void removeEgdesWithVertexTest() {
        int u = random.nextInt(N_VERTICES);
        int v = random.nextInt(N_VERTICES);
        int z = random.nextInt(N_VERTICES);

        graph.addVertex(u);
        graph.addVertex(v);
        graph.addVertex(z);

        graph.addEdge(u, v);
        graph.addEdge(u, z);
        assertTrue(graph.neighbours(u).size() >= 1);
        assertThrows(NullPointerException.class, () -> {
            graph.removeVertex(u);
            assertTrue(graph.neighbours(u).size() == 0);
        },
                "Removing a vertex should remove the egdes connencting the vertex");

    }

}

