def minimum_swaps_to_sort(arr):
    n = len(arr)
    # Create a list of tuples where each tuple is (value, index)
    arr_pos = [(arr[i], i) for i in range(n)]
    # Sort the array by the value of the elements
    arr_pos.sort(key=lambda it: it[0])
    # Create a visited array to mark visited elements
    visited = [False] * n
    swaps = []

    for i in range(n):
        # If the element is already visited or it is already in the correct position
        if visited[i] or arr_pos[i][1] == i:
            continue

        # Find out the number of nodes in this cycle
        cycle_size = 0
        j = i
        while not visited[j]:
            # Mark the node as visited
            visited[j] = True
            # Move to the next node
            next_node = arr_pos[j][1]
            swaps.append((j, next_node))
            j = next_node
            cycle_size += 1

        # If there is a cycle of size k, we need (k-1) swaps to sort it
        if cycle_size > 0:
            swaps = swaps[:-1]  # Remove the last swap as it is redundant

    return swaps


# Example usage
packages = [4, 3, 2, 1]
swaps = minimum_swaps_to_sort(packages)
print("Swaps needed to sort the packages:", swaps)

