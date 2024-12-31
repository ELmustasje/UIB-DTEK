# Answer File - Semester 2
# Description of each Implementation
Briefly describe your implementation of the different methods. What was your idea and how did you execute it? If there were any problems and/or failed implementations please add a description.

## Task 1 - mst
*to solve the mst problem i used "prims" algorithm witch i found here: https://www.geeksforgeeks.org/prims-minimum-spanning-tree-mst-greedy-algo-5/. The way it goes is: 1. you get a starting point. 2. You find all the edges and weight of you chosen vertex. 3. Take the minimum and add that edge to the mst. repeat 2 and 3 until all vertices are in the mst.
To keep track of the available edges and weight i used a priority que and poll-ed from that que (since that will be the lowest). Searched all the neighbours and added to the priority que. repeat until the number of vertices in the mst are equal to the number of vertices of the entire graph*

## Task 2 - lca
*to find the lowest common ansestor of two nodes i use bfs to build a parent map that records the parent of each node
then i construct the paths from node v and node u using the parents map. Lastly i reverse the path from the root and compare to find the deepest common node. This node will be the LCA*

## Task 3 - addRedundant
*For this task I couldnt find any well known standard algorithm, so I spent a lot of time trying and failing until I got a stately witch goes like this: create a hashmap of all the children, grandChildren of each node. traverse the mst with two nodes A and B and for each turn select the two biggest subtrees you can. Repeat this until you get to the situation that node A and B has the same amount of children as degree - 1


# Runtime Analysis
For each method of the different strategies give a runtime analysis in Big-O notation and a description of why it has this runtime.

**If you have implemented any helper methods you must add these as well.**

* ``mst(WeightedGraph<T, E> g)``: O(m * logn)
    * the while loop runs until all n vertices have been visited, and in each iteration, we are performing operations that involve the priority queue (pq) and addNewEdgesToQue which has a time complexity of O(k * logn) where k is the amount of neighbors and worst case there is m neighbours we have to visit to complete our tree.
* ``addNewEdgesToQue(PriorityQueue<Edge<V>> pq, WeightedGraph<V, E> g, V currentNode, Set<V> visited)``: O(n*logn)
  * for each adjacent edge of the current node, we check if the adjacent nodes have been visited, which takes O(1) time, and then add the edge to the priority queue, which takes O(logn) time. Since we do this for each edge, the overall time complexity is O(k*logn) where k is the amount of neighbours and worst case k = n - 1.
* ``lca(Graph<T> g, T root, T u, T v)``: O(n)
    * the initalization is O(1), since new HashMap(), New LinkedList(), new HashSet() and q.add(root) all are O(1). the while(!q.isEmpty) runs a BFS witch usually is O(m + n), but since a mst has n - 1 edges for n nodes we can simplify this to O(n). The path construction involves traversing upp the tree. in worst case this takes O(n). Path comparison, Collections.reverse takes O(n) and comparing takes worst case O(n).
    * combining all this will result on O(n)*
* ``dfs(V start, Graph<V> g,Map<V,Integer> childrenCount, Set<V> visited)``: O(n)
* The algorithm visits each vertex once and explores each edge once, resulting in a linear time complexity relative to the number of vertices n and edges m, and since in a mst m = n - 1 we can write it as O(n)

* ``Map<V,Integer> countChildrenInMst(Graph<V> g, V root)``: O(n)
* The method initializes data structures and performs a DFS traversal, resulting in linear time complexity relative to the number of vertices n  and edges m in the mst, and since m = n - 1 we can write it as O(n)
* ``getOptimalNode(Map<V,Integer> childrenCount, V start, Graph<V> g, Set<V> visited)``: O(n)
* The method iterates through vertices and their neighbors, potentially visiting each vertex and edge once, resulting in linear time complexity relative to the number of vertices n and edges m, and since m = n - 1 we can write O(n)
* ``addRedundant(Graph<V> g, V root)``: O(n)
* The method involves counting children, iterating over neighbors, and finding optimal nodes, all of which result in linear time complexity relative to the number of vertices n and edges m, and since m = n - 1 we can write O(n)
