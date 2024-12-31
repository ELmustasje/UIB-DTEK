package INF102.lab5.graph;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class AdjacencySetTest extends GraphTest {
    
    IGraph<Integer> adjacencySet;

    @BeforeEach
    public void setup() {
        adjacencySet = new AdjacencySet<>();
        for (int i = 0; i < N_VERTICES; i++) {
            adjacencySet.addVertice(i);
        }
    }
    
    @Test
    public void setAddEdgeToVerticeNotInGraph() {
        addEdgeToVerticeNotInGraph(adjacencySet);
    }

    @Test
    public void setAddEdgeTest() {
        addEdgeTest(adjacencySet);
    }

    @Test
    public void setRemoveEdgeTest() {
        removeEdgeTest(adjacencySet);
    }

    @Test
    public void setGetNeighbourhoodTest() {
        getNeighbourhoodTest(adjacencySet);
    }

    @Test
    public void setAddVerticeTest() {
        addVerticeTest(adjacencySet);
    }

    @Test
    public void setRemoveVerticeTest() {
        removeVerticeTest(adjacencySet);
    }
    
    @Test
    public void setDuplicateVerticeTest() {
        duplicateVerticeTest(adjacencySet);
    }
    @Test
    public void setRemoveEgdesWithVerticeTest(){
        removeEgdesWithVerticeTest(adjacencySet);
    }
}
