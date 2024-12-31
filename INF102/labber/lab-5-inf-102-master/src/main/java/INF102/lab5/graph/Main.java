package INF102.lab5.graph;


public class Main {

    public static final int N_VERTICES = 5;

    public static void main(String[] args) {
        IGraph<Integer> graph = new AdjacencySet<>();
        createGraph(graph);
        IGraphSearch<Integer> graphSearch = new GraphSearch<>(graph);
        
        int u = 0;
        int v = 2;

        System.out.println("----Expected to be True----");
        System.out.printf("%-17s| Edge %d and %d connected? %5b%n", graph.getClass().getSimpleName(), u, v, graphSearch.connected(u, v));

        v = 4;
        System.out.println("\n----Expected to be False----");
        System.out.printf("%-17s| Edge %d and %d connected? %5b%n", graph.getClass().getSimpleName(), u, v, graphSearch.connected(u, v));
    }

    /**
     * Creates a simple graph with 5 vertices.
     * 
     * 0 -> 1 -> 2      3 -> 4
     * 
     * @param graph
     */
    public static void createGraph(IGraph<Integer> graph) {
        graph.addVertice(0);
        graph.addVertice(1);
        graph.addVertice(2);
        graph.addVertice(3);
        graph.addVertice(4);
        
        graph.addEdge(0, 1);
        graph.addEdge(1, 2);
        graph.addEdge(3, 4);
    }
    
}
