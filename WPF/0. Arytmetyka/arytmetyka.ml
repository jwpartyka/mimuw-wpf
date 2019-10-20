(* Typ reprezentujący jednoczęściowy zbiór liczb. *)
type interval = {l : float; r : float}
type wartosc =
    | Pusty                         (* Zbiór przedział.      *)
    | Jeden of interval             (* Zbiór jednoczęściowy. *)
    | Dwa of interval * interval    (* Zbiór dwuczęściowy.   *)

let wartosc_dokladnosc x p =
    let procent = abs_float ((p /. 100.) *. x) in
    if x -. procent < 0. && x +. procent = 0. then
        Jeden {l = x -. procent; r = -0.}
    else Jeden {l = x -. procent; r = x +. procent}

let wartosc_od_do x y =
    if x < 0. && y = 0. then Jeden {l = x; r = -0.} else Jeden {l = x; r = y}

let wartosc_dokladna x =
    Jeden {l = x; r = x}

let in_wartosc x y =
    match x with
    | Jeden a -> a.l <= y && y <= a.r
    | Dwa (a, b) -> (a.l <= y && y <= a.r) || (b.l <= y && y <= b.r)
    | Pusty -> false

let min_wartosc x =
    match x with
    | Jeden a -> a.l
    | Dwa (a, _) -> a.l
    | Pusty -> nan

let max_wartosc x =
    match x with
    | Jeden a -> a.r
    | Dwa (_, b) -> b.r
    | Pusty -> nan

let sr_wartosc x =
    if x = Pusty then nan else
    let (mini, maks) = (min_wartosc x, max_wartosc x) in
    if mini = neg_infinity && maks = infinity then nan
    else (mini +. maks) /. 2.0

(* Procedura, która zwraca sumę zbiorów wartości niedokładnych x i y. *)
let rec union x y =
    (* Pomocnicza procedura, która sumuje dwa jednoczęściowe zbiory. *)
    let rec pom a b =
        if b.l < a.l then pom b a
        else if a.l <= b.l && b.r <= a.r then Jeden a
        else if b.l <= a.r then Jeden {a with r = b.r}
        else Dwa (a, b) in

    match x, y with
    | Jeden a, Jeden b -> pom a b
    | Jeden a, Dwa (c, d) -> union (union x (Jeden c)) (Jeden d)
    | Dwa (a, b), Jeden c -> union (Jeden a) (union (Jeden b) (Jeden c))
    | Dwa (a, b), Dwa (c, d) ->
        union (union (Jeden a) (Jeden c)) (union (Jeden b) (Jeden d))
    | Pusty, _ -> y
    | _, Pusty -> x

let rec plus x y =
    (* Pomocnicza procedura, która wykonuje operację plus a b *)
    (* na dwóch jednoczęściowych zbiorach a i b.              *)
    let pom a b =
        if a.r +. b.r = 0. then Jeden {l = a.l +. b.l; r = -0.}
        else Jeden {l = a.l +. b.l; r = a.r +. b.r} in

    match x, y with
    | Jeden a, Jeden c -> pom a c
    | Jeden a, Dwa (c, d) -> union (plus x (Jeden c)) (plus x (Jeden d))
    | Dwa (a, b), _ -> union (plus (Jeden a) y) (plus (Jeden b) y)
    | _, _ -> Pusty

let rec minus x y =
    (* Pomocnicza procedura, która wykonuje operację minus a b *)
    (* na dwóch jednoczęściowych zbiorach a i b.               *)
    let pom a b =
        if a.r -. b.l = 0. then Jeden {l = a.l -. b.r; r = -0.}
        else Jeden {l = a.l -. b.r; r = a.r -. b.l} in

    match x, y with
    | Jeden a, Jeden c -> pom a c
    | Jeden _, Dwa (c, d) -> union (minus x (Jeden c)) (minus x (Jeden d))
    | Dwa (a, b), _-> union (minus (Jeden a) y) (minus (Jeden b) y)
    | _, _ -> Pusty

let rec razy x y =
    (* Pomocnicza procedura, która wykonuje operację razy a b *)
    (* na dwóch jednoczęściowych zbiorach a i b.              *)
    let rec pom a b =
        if a = {l = 0.; r = 0.} || b = {l = 0.; r = 0.} then
            Jeden {l = 0.; r = 0.}
        else if a.l *. a.r < 0. then
            union (pom {a with r = -0.} b) (pom {a with l = 0.} b)
        else if b.l *. b.r < 0. then
            union (pom a {b with r = -0.}) (pom a {b with l = 0.})
        else if a.l < 0. then
            if b.r <= 0. then Jeden {l = a.r *. b.r; r = a.l *. b.l}
            else Jeden {l = a.l *. b.r; r = a.r *. b.l}
        else
            if b.r <= 0. then Jeden {l = a.r *. b.l; r = a.l *. b.r}
            else Jeden {l = a.l *. b.l; r = a.r *. b.r} in

    match x, y with
    | Jeden a, Jeden c -> pom a c
    | Jeden _, Dwa (c, d) -> union (razy x (Jeden c)) (razy x (Jeden d))
    | Dwa (a, b), _ -> union (razy (Jeden a) y) (razy (Jeden b) y)
    | _, _ -> Pusty

let rec podzielic x y =
    (* Pomocnicza procedura, która wykonuje operację podzielic a b *)
    (* na dwóch jednoczęściowych zbiorach a i b .                  *)
    let rec pom a b =
        if b = {l = 0.; r = 0.} then Pusty
        else if a = {l = 0.; r = 0.} then Jeden {l = 0.; r = 0.}
        else if a.l *. a.r < 0. then
            union (pom {a with r = -0.} b) (pom {a with l = 0.} b)
        else if b.l *. b.r < 0. then
            union (pom a {b with r = -0.}) (pom a {b with l = 0.})
        else if a.l < 0. then
            if b.r <= 0. then Jeden {l = a.r /. b.l; r = a.l /. b.r}
            else Jeden {l = a.l /. b.l; r = a.r /. b.r}
        else
            if b.r <= 0. then Jeden {l = a.r /. b.r; r = a.l /. b.l}
            else Jeden {l = a.l /. b.r; r = a.r /. b.l} in

    match x, y with
    | Jeden a, Jeden c -> pom a c
    | Jeden _, Dwa (c, d) ->
        union (podzielic x (Jeden c)) (podzielic x (Jeden d))
    | Dwa (a, b), _ -> union (podzielic (Jeden a) y) (podzielic (Jeden b) y)
    | _, _ -> Pusty
