(* Autor: Janusz Partyka               *)
(* Code reviewer: Tymoteusz Wisniewski *)

(* W tym programie wartosc npl wierzcholka v oznacza "null path length",    *)
(* czyli liczbe wezlow pomiedzy wezlem v a najblizszym mu wezlem typu Null. *)
(* npl(Null) = -1                                                           *)
(* npl(lisc) = 0                                                            *)
(* npl(wezel z jednym synem) = 0                                            *)

(* Typ zlaczalnej kolejki priorytetowej *)
type 'a queue =
    (* Pusty wezel *)
    | Null
    (* Wezel (priorytet, lewe poddrzewo, prawe poddrzewo, npl) *)
    | Node of 'a * 'a queue * 'a queue * int

(* Wyjatek podnoszony przez [delete_min], gdy kolejka jest pusta *)
exception Empty

(* Pusta kolejka priorytetowa *)
let empty = Null

(* [join q1 q2] zwraca zlaczenie kolejek [q1] i [q2] *)
let rec join q1 q2 =
    (* Procedura, ktora zwraca npl wezla v *)
    let npl v =
        match v with
        | Null -> -1
        | Node (_, _, _, x) -> x in
    match q1, q2 with
    | Node (a_pri, a_l, a_r, _), Node (b_pri, _, _, _) ->
        if b_pri < a_pri then
            join q2 q1
        else
            let new_r = join a_r q2 in
            if npl a_l < npl new_r then
                Node (a_pri, new_r, a_l, npl a_l + 1)
            else
                Node (a_pri, a_l, new_r, npl new_r + 1)
    | Node _, Null -> q1
    | _, _ -> q2

(* [add e q] zwraca kolejke powstala z dolaczenia elementu [e] *)
(* do kolejki [q]                                              *)
let add e q =
    join q (Node (e, Null, Null, 0))

(* Dla niepustej kolejki [q], [delete_min q] zwraca pare [(e,q')], gdzie [e] *)
(* jest elementem minimalnym kolejki [q], a [q'] to [q] bez elementu [e].    *)
(* Jesli [q] jest puste podnosi wyjatek [Empty].                             *)
let delete_min q =
    match q with
    | Null -> raise Empty
    | Node (pri, l, r, _) -> (pri, join l r)

(* Zwraca [true] jesli kolejka [q] jest pusta. W przeciwnym razie [false] *)
let is_empty q =
    q = Null

(* Testy autorstwa Marcina Wawerki *)
(*
let a = empty;;
let b = add 1 empty;;

assert (is_empty a = true);;
assert (try let _=delete_min a in false with Empty -> true);;
assert (is_empty b <> true);;

let b = add 1 empty;;
let b = add 3 b;;
let b = add (-1) b;;
let b = add 2 b;;
let b = add 1 b;;

let (a,b) = delete_min b;;
assert (a = -1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let b = add "a" empty;;
let b = add "aca" b;;
let b = add "nzbzad" b;;
let b = add "nzbza" b;;
let b = add "bxbxc" b;;

let (a,b) = delete_min b;;
assert (a = "a");;

let (a,b) = delete_min b;;
assert (a = "aca");;

let b = add 1 empty;;
let b = add 3 b;;
let b = add (-1) b;;
let b = add 2 b;;
let b = add 1 b;;

let c = add 10 empty;;
let c = add (-5) c;;
let c = add 1 c;;
let c = add 4 c;;
let c = add 0 c;;

let b = join b c;;

let (a,b) = delete_min b;;
assert (a = (-5));;

let (a,b) = delete_min b;;
assert (a = (-1));;

let (a,b) = delete_min b;;
assert (a = 0);;
*)
