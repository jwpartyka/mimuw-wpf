type 'a queue =
    | Pusta
    | Node of 'a * 'a queue * 'a queue

exception Empty

let empty = Pusta

let init x =
    Node (x, Pusta, Pusta)

let rec join q1 q2 =
    match q1, q2 with
    | Node (apri, al, ar), Node (bpri, bl, br) ->
        if bpri < apri then join q2 q1
        else Node (apri, al, join ar q2)
    | Node _, Pusta -> q1
    | Pusta, Node _ -> q2
    | Pusta, Pusta -> Pusta

let add e q =
    join q (init e)

let delete_min q =
    match q with
    | Pusta -> raise (Empty)
    | Node (pri, l, r) -> (pri, join l r)

let is_empty q =
    q = Empty
