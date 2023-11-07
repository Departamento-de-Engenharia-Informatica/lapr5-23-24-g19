### USER STORY 510

**Description**

>As a task manager, I intend to find paths between buildings that aim to optimize a given criterion, for example,
minimizing the number of elevator uses or passing through the smallest number of buildings.
The paths within each building's corridor should attempt to minimize the distance traveled and be
generated from a graph between rectangular 'cells' positioned in the corridor, with there always being a cell next
to each office or room access.

To develop a solution in Prolog for finding paths between buildings, optimizing
criteria such as minimizing elevator usage and passing through the fewest number of buildings,
we can start by defining the problem in terms of a graph representation.
Paths within building corridors should minimize the distance traveled and be generated from a
graph of rectangular cells positioned in the corridor.

Here's how you might approach this problem in Prolog:

**Define the Graph:**

- Represent the layout of the buildings and corridors using a grid of cells (rectangular or square) in Prolog facts or predicates.
- Define relationships (edges) between cells that represent valid movement between them.

**Define Criteria:**

- Implement rules and predicates to define the optimization criteria, such as minimizing elevator usage or passing through the fewest number of buildings. Specify how these criteria are scored or measured.

**Implement Pathfinding Algorithms:**

- Implement pathfinding algorithms like Depth-First Search (DFS), Breadth-First Search (BFS), and A* search to find paths through the grid.
- Use standard Prolog constructs and recursion to implement these algorithms.

**Track and Compare Paths:**

- Use Prolog predicates to track and compare different paths based on the defined criteria. You might use assert/retract predicates to store and update information about the best paths found.

**Query for Paths:**

- Write queries in Prolog to request paths between specific locations or buildings based on the criteria.
