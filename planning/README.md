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



## User Story 1410

### What to improve on the genetic algorithm

1. Ensure that at least the best individual between the current population and the new generated population moves on to the next population.

2. Avoid having the sequence of crossovers always occur between the 1st and 2nd elements of the population, then between the 3rd and 4th, 5th and 6th, and so on.

3. Apply a selection method that is not purely elitist, giving some probability that an individual with a worse evaluation can pass to the next generation, even if it is not among the DP (population size) best-evaluated individuals from the previous population and their descendants.

4. The termination condition can be different (in addition to being based on the number of generations, consider other possibilities such as time, stabilization of the solution, etc.).

5. The provided Genetic Algorithm (GA) replaces the current population pair by pair with their descendants. There is no guarantee that the best individual will move to the next generation.

   - If it is desired that the best, or some of the best, individuals move to the next generation, simply find them in a list that combines the elements of the previous population with their descendants obtained through crossover and mutation.

   - Subsequently, the selection of the remaining individuals from the list can be done by some method that is not purely elitist. We'll suggest a method.

6. The provided GA always tries to mate the 1st individual with the 2nd, the 3rd with the 4th, and so on. A simple way to avoid this problem is to perform a random permutation between the elements of the population before using the implemented crossover. Afterward, there won't be an issue in mating the 1st and 2nd, 3rd and 4th, etc., because they won't be the same as before.

7. How to perform a random permutation between individuals in a population?

   - Use the `random_permutation/2` predicate from SWI-Prolog.