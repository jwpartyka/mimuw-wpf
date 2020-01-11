(* Zadanie: Sortowanie topologiczne *)
(* Autor: Janusz Partyka            *)
(* Code review: Krzysztof Bartoszek *)

open PMap

(* Wyjątek podnoszony przez topol, gdy zależności są cykliczne. *)
exception Cykliczne

(* Dla danej listy [(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])] *)
(* zwraca listę, na której każdy z elementów a_i oraz a_ij występuje   *)
(* dokładnie raz i która jest uporządkowana w taki sposób, że każdy    *)
(* element a_i jest przed każdym z elementów a_i1 ... a_il.            *)
let topol edges =
  (* Procedura, która do listy sąsiadów wierzchołka v w mapie m            *)
  (* dodaje wszystkie wierzchołki z listy l i zwraca zaktuazlizowaną mapę. *)
  let add_edges g (v, l) =
    if exists v g then
      let (vis, adj) = find v g in
      add v (vis, l @ adj) g
    else add v (0, l) g in

  (* Procedura przetwarzająca wierzchołek v w DFS i wywołująca się na       *)
  (* wszystkich jego sąsiadach. Po przetworzeniu, wierzchołek jest dodawany *)
  (* do listy topo, która, po przetworzeniu całego grafu, będzie listą      *)
  (* wszystkich wierzchołków grafu w porządku topologicznym.                *)
  let rec dfs (topo, graph) v =
    let (vis, adj) = find v graph in
    if vis = 0 then
      let (topo, graph) = List.fold_left dfs (topo, add v (1, adj) graph) adj in
      (v :: topo, add v (2, []) graph)
    else if vis = 1 then raise Cykliczne
    else (topo, graph) in

  let tails = List.map fst edges
  and heads = List.flatten (List.map snd edges) in
  let graph = List.fold_left (fun g v -> add v (0, []) g) empty heads in
  let graph = List.fold_left add_edges graph edges in
  List.fold_left dfs ([], graph) tails |> fst
