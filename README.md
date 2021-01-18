# wolfram_steiner
## Wolfram Mathematica steiner tree problem solving package.

Exact methods:
1. Dreyfus-Wagner algorithm, straight dynamic realisation O(n 3^t + n^2 2^t + poly())

Approximation:
1. Kou-Markowsky-Berman 2-approximation algorithm O(|T| n log(m)).
2. Mehlhorn 2-approximation algorithm O(n log(m)).

Greedy:
1. (SPH) Shortest path heuristic O(|T| n log(m)).
2. (RSPH) Repeated shortest path heuristic O(|it| |T| n log(m)).

Local-search:
1. Steiner-vertex insertion.
2. Steiner-vertex elimination.
3. Key-path exchange.