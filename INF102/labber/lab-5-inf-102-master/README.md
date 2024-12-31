# Lab 5 - Undirected Graphs
In this task you will be coding a [graph](https://en.wikipedia.org/wiki/Graph_(discrete_mathematics)) datastructure implementation **Adjacency Set** which is an Adjacency List that uses sets instead of lists.

The graphs in this assignment are **undirected** and **unweighted**:

`Main.java` contains some code to do simple checks for the graph implementation.

### Task 1
Implement the remaining methods in `AdjacencySet.java`. Methods:
 * `addVertice(V vertice)`
 * `removeVertice(V vertice)`
 * `addEdge(V u, v v)`
 * `removeEdge(V u, V v)`
 * `hasVertice(V vertice)`

✅ Run `AdjacencySetTest` to check your solution. The task is passed if all tests pass.


### Task 2
An essential part of graph theory is to be able to search through a graph. In this task we want to know whether two vertices `u` and `v` are connected.

<img align="center" src="images/Lab5_graph.png" width="400"/>

In the graph above `0` and `2` are connected since there is a direct edge between them. `3` and `2` are also connected since there are vertices inbetween that connect them. <br></br>
While `2` and `5`  are not connected since there are no edges that directly connect them or any inbetween other vertices.

**TODO: Implement `GraphSearch::connected`.**

✅ Run `GraphSearchTest` to check your solution. The task is passed if all tests pass.


