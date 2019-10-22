(* Typ reprezentujący jednoczęściowy zbiór liczb. *)
type interval = {l : float; r : float}

(* Typ reprezentujący niedokładne wartości. *)
type wartosc =
    (* Zbiór pusty.          *)
    | Pusty
    (* Zbiór jednoczęściowy. *)
    | Jeden of interval
    (* Zbiór dwuczęściowy.   *)
    | Dwa of interval * interval

(* wartosc_dokladnosc x p = [x - p% z x; x + p% z x] *)
(* war. pocz.: p > 0                                 *)
let wartosc_dokladnosc x p =
    let procent = abs_float ((p /. 100.) *. x) in
    if x -. procent < 0. && x +. procent = 0. then
        Jeden {l = x -. procent; r = -0.}
    else Jeden {l = x -. procent; r = x +. procent}

(* wartosc_od_do x y = [x; y] *)
(* war. pocz.: x <= y         *)
let wartosc_od_do x y =
    if x < 0. && y = 0. then Jeden {l = x; r = -0.}
    else Jeden {l = x; r = y}

(* wartosc dokladna x = [x; x] *)
let wartosc_dokladna x =
    Jeden {l = x; r = x}

(* in_wartosc x y = wartość x może być równa y *)
let in_wartosc x y =
    match x with
    | Jeden a -> a.l <= y && y <= a.r
    | Dwa (a, b) -> (a.l <= y && y <= a.r) || (b.l <= y && y <= b.r)
    | Pusty -> false

(* min_wartosc x = najmniejsza możliwa wartość x,    *)
(* lub neg_infinity jeśli brak dolnego ograniczenia. *)
let min_wartosc x =
    match x with
    | Jeden a -> a.l
    | Dwa (a, _) -> a.l
    | Pusty -> nan

(* max_wartosc x = największa możliwa wartość x, *)
(* lub infinity jeśli brak górnego ograniczenia. *)
let max_wartosc x =
    match x with
    | Jeden a -> a.r
    | Dwa (_, b) -> b.r
    | Pusty -> nan

(* sr_wartosc x = srednia arytmetyczna min_wartosc x i max_wartosc x *)
(* lub nan jeśli min_wartosc x i max_wartosc x nie są skończone.     *)
let sr_wartosc x =
    if x = Pusty then nan
    else
        let (mini, maks) = (min_wartosc x, max_wartosc x) in
        if mini = neg_infinity && maks = infinity then nan
        else (mini +. maks) /. 2.

(* union x y = {t : in_wartosc x t lub in_wartosc y t} *)
(* x, y - wartości niedokładne                         *)
let rec union x y =
    (* pom a b = union (Jeden a) (Jeden b) *)
    (* a i b są typu interval.             *)
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

(* Procedura pomocnicza do procedur plus/minus/razy/podzielic, *)
(* która wykonuje procedurę f na parach jednoczęściowych       *)
(* zbiorów, takich, że jeden zawiera się w x, a drugi w y.     *)
(* f - procedura pomocnicza procedur plus/minus/razy/podzielic *)
(* x, y - wartości niedokładne     .                           *)
let rec operacja f x y =
    match x, y with
    | Jeden a, Jeden c -> f a c
    | Jeden a, Dwa (c, d) ->
        union (operacja f x (Jeden c)) (operacja f x (Jeden d))
    | Dwa (a, b), _ ->
        union (operacja f (Jeden a) y) (operacja f (Jeden b) y)
    | _, _ -> Pusty

(* plus x y = {a + b : in_wartosc x a i in_wartosc y b} *)
let plus x y =
    (* pom a b = plus (Jeden a) (Jeden b) *)
    (* a i b są typu interval.            *)
    let pom a b =
        if a.r +. b.r = 0. then Jeden {l = a.l +. b.l; r = -0.}
        else Jeden {l = a.l +. b.l; r = a.r +. b.r} in
    operacja pom x y

(* minus x y = {a - b : in_wartosc x a i in_wartosc y b} *)
let minus x y =
    (* pom a b = minus (Jeden a) (Jeden b) *)
    (* a i b są typu interval.             *)
    let pom a b =
        if a.r -. b.l = 0. then Jeden {l = a.l -. b.r; r = -0.}
        else Jeden {l = a.l -. b.r; r = a.r -. b.l} in
    operacja pom x y

(* razy x y = {a * b : in_wartosc x a i in_wartosc y b} *)
let razy x y =
    (* pom a b = razy (Jeden a) (Jeden b)                          *)
    (* a i b są typu interval.                                     *)
    (* Jeżeli, zbiór (l, r) zawiera i ujemne, i dodatnie wartości, *)
    (* to zostaje on podzielony na dwa zbiory: (l; -0.) i (0.; r). *)
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
    operacja pom x y

(* podzielic x y = {a / b : in_wartosc x a i in_wartosc y b} *)
let podzielic x y =
    (* pom a b = podzielic (Jeden a) (Jeden b)                     *)
    (* a i b są typu interval.                                     *)
    (* Jeżeli, zbiór (l, r) zawiera i ujemne, i dodatnie wartości, *)
    (* to zostaje on podzielony na dwa zbiory: (l; -0.) i (0.; r). *)
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
    operacja pom x y
