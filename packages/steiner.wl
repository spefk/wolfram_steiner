(* ::Package:: *)

(* Package to load all other packages. *)


Needs["Steiner`Utilities`", "steiner_utilities.wl"]


Needs["LeftistHeap`", "data_structures\\leftist_heap.wl"]


Needs["Steiner`Algorithms`GraphUtilities`", "steiner_algorithms_graph_utilities.wl"]


Needs["Steiner`Generator`", "steiner_generator.wl"]
Needs["Steiner`SteinLib`", "steiner_steinlib.wl"]


Needs["Steiner`Visualization`", "steiner_visualization.wl"]


Needs["Steiner`Algorithms`Exact`", "steiner_algorithms_exact.wl"]


Needs["Steiner`Algorithms`Dijkstra`", "steiner_algorithms_dijkstra.wl"]
Needs["Steiner`Algorithms`Greedy`", "steiner_algorithms_greedy.wl"]


Needs["Steiner`Algorithms`Voronoi`", "steiner_algorithms_voronoi.wl"]
Needs["Steiner`Algorithms`Approximation`", "steiner_algorithms_approximation.wl"]
Needs["Steiner`Algorithms`LocalSearch`", "steiner_algorithms_local_search.wl"]


Needs["TestingSystem`", "testing_system.wl"]


BeginPackage["Steiner`"];


EndPackage[]
