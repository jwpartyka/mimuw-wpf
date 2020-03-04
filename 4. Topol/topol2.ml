(* Zadanie: Sortowanie topologiczne *)
(* Autor: Janusz Partyka            *)

open List
open PMap

(* Wyjątek podnoszony przez topol, gdy zależności są cykliczne *)
exception Cykliczne

(* Dla danej listy [(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])]
    zwraca liste, na ktorej kazdy z elementow a_i oraz a_ij wystepuje
    dokladnie raz i ktora jest uporzadkowana w taki sposob, ze kazdy
    element a_i jest przed kazdym z elementow a_i1 ... a_il *)
let topol lista = begin
    (* Procedura, która do listy sąsiadów wierzchołka v w mapie m           *)
    (* dodaje wszystkie wierzchołki z listy l i zwraca zaktuazlizowaną mapę *)
    let add_edges m (v, l) =
        if exists v m then
            let adj = find v m in
            add v (l @ adj) m
        else add v l m in

    (* Procedura, która inkrementuje stopień wierzchołka v na mapie m *)
    (* i zwraca zaktualizowaną mapę                                   *)
    let inc_degree m v =
        if exists v m then add v (find v m + 1) m
        else add v 1 m in

    (* Procedura, która dekrementuje stopień wierzchołka v i zwraca        *)
    (* aktualizowaną mapę stopni oraz listę wierzchołków o zerowym stopniu *)
    let pom (stack, indeg) v =
        let deg = find v indeg in
        if deg = 1 then (v :: stack, add v (deg - 1) indeg)
        else (stack, add v (deg - 1) indeg) in

    let rec process s indeg edges acc =
        match s with
        | [] ->
            if is_empty edges then acc
            else raise Cykliczne
        | (v :: l) ->
            let (s, indeg) = fold_left pom (l, indeg) (find v edges) in
            process s (remove v indeg) (remove v edges) (v :: acc) in

    let zero_deg v deg a =
        if deg = 0 then v :: a
        else a in

    let wyj = List.map fst lista
    and wej = flatten (List.map snd lista) in
    let indeg = fold_left (fun a x -> add x 0 a) empty wyj in
    let edges = fold_left (fun a x -> add x [] a) empty wej in
    let indeg = fold_left inc_degree indeg wej in
    let edges = fold_left add_edges edges lista in
    let st = foldi zero_deg indeg [] in
    process st indeg edges [] |> rev
end
