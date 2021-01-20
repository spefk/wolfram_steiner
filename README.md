## Wolfram Mathematica steiner tree problem solving package.

**Under construction.**

*Given graph G = (V, E) and set of terminals T as instance of steiner tree problem, let n = #V, m = #E, t = #T.*

### Implemented algorithms:
Exact methods [1][2]:
1. (DW) Dreyfus-Wagner algorithm, straight dynamic realisation O(3^t n + 2^t n^2)

Approximation [1][3][5]:
1. (DNH) Kou-Markowsky-Berman 2-approximation algorithm O(t m log n).
2. (DNHA) Mehlhorn 2-approximation algorithm O(m log n).

Greedy [4]:
1. (SPH) Shortest path heuristic O(n m log(n)).
2. (RSPH) Repeated shortest path heuristic O(n m log n).

Local-search [4]:
1. (VI) Steiner-vertex insertion.
2. (VE) Steiner-vertex elimination.
3. (PE) Key-path exchange.


[0] testing data was taken from http://steinlib.zib.de
[1] Корте Б. Комбинаторная оптимизация. Теория и Алгоритмы / Бернард Корте, Йенс Фиген ; перевод с англ. М.А. Бабенко ; - М. : МЦНМО ; 2015 - 720 с.
[2] Dreyfus S.  The Steiner Problem in Graphs / S. Dreyfus, R. Wagner ; 1971
[3] Markowski G. A fast algorithm for steiner trees / G. Markowski, L. Kou, L. Berman ; 1981.
[4] Uchoa E. Fast Local Search for Steiner Trees in Graphs / E. Uchoa, R. Werneck  ; 2010
[5] Mehlhorn K. A Faster Approximation Algorithm for the Steiner Tree Problem in Graphs / K. Mehlhorn ; 1988
