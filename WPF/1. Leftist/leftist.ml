(* W tym programie wartość npl wierzchołka v oznacza "null path length", *)
(* czyli liczbę węzłów pomiędzy węzłem v a najbliższym węzłem typu Null. *)
(* npl(Null) = -1                                                        *)
(* npl(liść) = 0                                                         *)
(* npl(węzeł z jednym synem) = 0                                         *)

(* Typ złączalnej kolejki priorytetowej *)
type 'a queue =
    (* Pusty węzeł *)
    | Null
    (* Węzeł (priorytet, lewe poddrzewo, prawe poddrzewo, npl) *)
    | Node of 'a * 'a queue * 'a queue * int

(* Wyjątek podnoszony przez [delete_min] gdy kolejka jest pusta *)
exception Empty

(* Pusta kolejka priorytetowa *)
let empty = Null

(* [join q1 q2] zwraca złączenie kolejek [q1] i [q2] *)
let rec join q1 q2 =
    (* Procedura, która zwraca npl węzła v *)
    let npl v =
        match v with
        | Null -> -1
        | Node (_, _, _, x) -> x in
    match q1, q2 with
    | Node (a_pri, a_l, a_r, _), Node (b_pri, _, _, _) ->
        if b_pri < a_pri then join q2 q1
        else
            let new_r = join a_r q2 in
            if npl a_l < npl new_r then Node (a_pri, new_r, a_l, npl a_l + 1)
            else Node (a_pri, a_l, new_r, npl new_r + 1)
    | Node _, Null -> q1
    | _, _ -> q2

(* [add e q] zwraca kolejkę powstałą z dołączenia elementu [e] *)
(* do kolejki [q]                                              *)
let add e q =
    join q (Node (e, Null, Null, 0))

(* Dla niepustej kolejki [q], [delete_min q] zwraca parę [(e,q')] gdzie [e] *)
(* jest elementem minimalnym kolejki [q] a [q'] to [q] bez elementu [e].    *)
(* Jeśli [q] jest puste podnosi wyjątek [Empty].                            *)
let delete_min q =
    match q with
    | Null -> raise Empty
    | Node (pri, l, r, _) -> (pri, join l r)

(* Zwraca [true] jeśli dana kolejka jest pusta. W przeciwnym razie [false] *)
let is_empty q =
    q = Null
