from collections import defaultdict, deque


def parse_input(input_text):
    sections = input_text.strip().split("\n\n")
    ordering_rules = [
        tuple(map(int, line.split("|"))) for line in sections[0].splitlines()
    ]
    updates = [list(map(int, line.split(","))) for line in sections[1].splitlines()]
    return ordering_rules, updates


def is_update_correct(ordering_rules, update):
    # Convert update to a position map for quick lookup
    position = {page: idx for idx, page in enumerate(update)}
    for x, y in ordering_rules:
        # If both X and Y are in the update, check their order
        if x in position and y in position and position[x] > position[y]:
            return False
    return True


def reorder_update(ordering_rules, update):
    # Build a directed graph for the rules
    graph = defaultdict(list)
    in_degree = defaultdict(int)
    pages_in_update = set(update)

    # Add edges for rules that apply to the current update
    for x, y in ordering_rules:
        if x in pages_in_update and y in pages_in_update:
            graph[x].append(y)
            in_degree[y] += 1

    # Topological sort using Kahn's algorithm
    queue = deque([page for page in update if in_degree[page] == 0])
    ordered_update = []

    while queue:
        current = queue.popleft()
        ordered_update.append(current)
        for neighbor in graph[current]:
            in_degree[neighbor] -= 1
            if in_degree[neighbor] == 0:
                queue.append(neighbor)

    return ordered_update


def find_middle_page(update):
    n = len(update)
    return update[n // 2]


def main(input_text):
    ordering_rules, updates = parse_input(input_text)
    total_middle_pages = 0

    for update in updates:
        if not is_update_correct(ordering_rules, update):
            # Reorder the update
            corrected_update = reorder_update(ordering_rules, update)
            total_middle_pages += find_middle_page(corrected_update)

    return total_middle_pages


f = open("input.txt")
l = "".join(f.readlines())
print(main(l))
